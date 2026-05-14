# modules/tab_ccot_outcomes.R
# Tab 2: CCOT Outcomes — transfer rates by window, recommendation category,
# PEWS trajectory, and time-of-day analyses.

ccotOutcomesUI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "CCOT Outcomes",
    icon  = bsicons::bs_icon("clipboard-pulse"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Filters",
        selectInput(ns("unit"), "Unit",
                    choices = c("All", FLOOR_UNITS), selected = "All"),
        selectInput(ns("rec"), "Recommendation",
                    choices = c("All", RECOMMENDATION_LEVELS),
                    selected = "All"),
        checkboxGroupInput(ns("shift"), "Time of day (T0)",
                           choices = SHIFT_BUCKETS$shift,
                           selected = SHIFT_BUCKETS$shift),
        checkboxGroupInput(ns("dow"), "Day of week (T0)",
                           choices = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                           selected = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
      ),
      uiOutput(ns("kpis")),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Cumulative transfer rate by window"),
          plotly::plotlyOutput(ns("funnel"), height = "320px")
        ),
        bslib::card(
          bslib::card_header("Emergency transfer rate by recommendation"),
          plotly::plotlyOutput(ns("by_rec"), height = "320px")
        )
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          bslib::card_header("Mean composite PEWS — 24h before T0"),
          plotly::plotlyOutput(ns("pews_traj"), height = "340px")
        ),
        bslib::card(
          bslib::card_header("CCOT note hour-of-day — transferred vs not"),
          plotly::plotlyOutput(ns("hour_outcome"), height = "340px")
        )
      ),
      bslib::card(
        bslib::card_header("CCOT cases (filtered)"),
        DT::DTOutput(ns("table"))
      )
    )
  )
}

ccotOutcomesServer <- function(id, date_range) {
  moduleServer(id, function(input, output, session) {

    cases_f <- reactive({
      df <- filter_by_date(ccot_outcomes, "T0", date_range())
      if (!is.null(input$unit) && input$unit != "All") {
        df <- df[df$unit == input$unit, , drop = FALSE]
      }
      if (!is.null(input$rec) && input$rec != "All") {
        df <- df[df$recommendation_category == input$rec, , drop = FALSE]
      }
      if (!is.null(input$shift)) {
        df <- df[as.character(df$T0_shift) %in% input$shift, , drop = FALSE]
      }
      if (!is.null(input$dow)) {
        df <- df[as.character(df$T0_dow) %in% input$dow, , drop = FALSE]
      }
      df
    })

    output$kpis <- renderUI({
      df <- cases_f()
      n  <- nrow(df)
      windowed_rate <- function(h) {
        if (!n) return("—")
        scales::percent(
          mean(!is.na(df$hours_to_transfer) &
                 df$hours_to_transfer >= 0 &
                 df$hours_to_transfer <= h),
          accuracy = 0.1
        )
      }
      emerg_rate <- if (n) {
        scales::percent(mean(df$emergency_transfer), accuracy = 0.1)
      } else "—"

      bslib::layout_columns(
        fill = FALSE,
        kpi_box("CCOT cases (filtered)", n, NULL, icon = "people"),
        kpi_box("Transfer within 2h",  windowed_rate(2),  NULL,
                icon = "stopwatch"),
        kpi_box("Transfer within 12h", windowed_rate(12), NULL,
                icon = "stopwatch"),
        kpi_box("Transfer within 24h", windowed_rate(24), NULL,
                icon = "stopwatch"),
        kpi_box("Transfer within 48h", windowed_rate(48), NULL,
                icon = "stopwatch"),
        kpi_box("Emergency transfer (72h)", emerg_rate,
                "Intubation or vasopressor",
                icon = "heart-pulse", theme_color = "danger")
      )
    })

    output$funnel <- plotly::renderPlotly({
      df <- cases_f()
      validate(need(nrow(df) > 0, "No CCOT cases match filters."))
      rates <- purrr::map_dfr(TRANSFER_WINDOWS_HOURS, function(h) {
        tibble::tibble(
          window_h = h,
          rate = mean(!is.na(df$hours_to_transfer) &
                        df$hours_to_transfer >= 0 &
                        df$hours_to_transfer <= h)
        )
      })
      p <- ggplot(rates, aes(factor(window_h), rate)) +
        geom_col(fill = "#2c7fb8") +
        geom_text(aes(label = scales::percent(rate, 0.1)),
                  vjust = -0.4, size = 3.5) +
        scale_y_continuous(labels = scales::percent_format(1),
                           expand = expansion(mult = c(0, 0.15))) +
        labs(x = "Hours after T0", y = "Cumulative transfer rate") +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y")))
    })

    output$by_rec <- plotly::renderPlotly({
      df <- cases_f()
      validate(need(nrow(df) > 0, "No CCOT cases match filters."))
      sm <- df |>
        dplyr::group_by(recommendation_category) |>
        dplyr::summarise(
          n = dplyr::n(),
          emergency_rate = mean(emergency_transfer),
          .groups = "drop"
        )
      p <- ggplot(sm, aes(forcats::fct_reorder(recommendation_category,
                                               emergency_rate),
                          emergency_rate, fill = recommendation_category)) +
        geom_col() +
        geom_text(aes(label = sprintf("%s\n(n=%d)",
                                      scales::percent(emergency_rate, 0.1), n)),
                  vjust = -0.2, size = 3.2) +
        scale_y_continuous(labels = scales::percent_format(1),
                           expand = expansion(mult = c(0, 0.2))) +
        labs(x = NULL, y = "Emergency transfer rate") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y")))
    })

    output$pews_traj <- plotly::renderPlotly({
      df <- cases_f()
      validate(need(nrow(df) > 0, "No CCOT cases match filters."))

      # Join flowsheet rows to T0 and bucket into hour bins relative to T0.
      pf <- flowsheet_rows |>
        dplyr::inner_join(
          df |> dplyr::select(pat_enc_csn, T0, within_window,
                              emergency_transfer, hours_to_transfer),
          by = "pat_enc_csn"
        ) |>
        dplyr::mutate(
          rel_hour = floor(hours_between(flowsheet_datetime, T0)),
          transferred = !is.na(hours_to_transfer) &
                          hours_to_transfer >= 0 &
                          hours_to_transfer <= OBSERVATION_WINDOW_HOURS
        ) |>
        dplyr::filter(rel_hour >= -24, rel_hour <= 0)

      validate(need(nrow(pf) > 0, "No PEWS rows in 24h pre-T0 window."))

      sm <- pf |>
        dplyr::group_by(rel_hour, transferred) |>
        dplyr::summarise(mean_pews = mean(pews_total),
                         n = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(group = ifelse(transferred,
                                     "Transferred ≤72h",
                                     "Not transferred"))

      p <- ggplot(sm, aes(rel_hour, mean_pews, color = group)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.5) +
        scale_color_manual(values = c("Transferred ≤72h" = "#d62728",
                                      "Not transferred"  = "#2ca02c")) +
        labs(x = "Hours relative to T0", y = "Mean composite PEWS",
             color = NULL) +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "colour")))
    })

    output$hour_outcome <- plotly::renderPlotly({
      df <- cases_f()
      validate(need(nrow(df) > 0, "No CCOT cases match filters."))
      sm <- df |>
        dplyr::mutate(
          hour = floor(T0_hour),
          transferred = !is.na(hours_to_transfer) &
                          hours_to_transfer >= 0 &
                          hours_to_transfer <= OBSERVATION_WINDOW_HOURS
        ) |>
        dplyr::count(hour, transferred, name = "n") |>
        dplyr::mutate(group = ifelse(transferred,
                                     "Transferred ≤72h",
                                     "Not transferred"))
      p <- ggplot(sm, aes(hour, n, fill = group)) +
        geom_col(position = "stack") +
        scale_fill_manual(values = c("Transferred ≤72h" = "#d62728",
                                     "Not transferred"  = "#2ca02c")) +
        scale_x_continuous(breaks = seq(0, 23, by = 3)) +
        labs(x = "Hour of CCOT note (T0)", y = "Cases", fill = NULL) +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "fill")))
    })

    output$table <- DT::renderDT({
      df <- cases_f() |>
        dplyr::transmute(
          pat_enc_csn, unit, T0,
          T0_shift, T0_dow,
          recommendation_category,
          hours_to_transfer = round(hours_to_transfer, 1),
          emergency_transfer
        )
      DT::datatable(df, rownames = FALSE, filter = "top",
                    options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}
