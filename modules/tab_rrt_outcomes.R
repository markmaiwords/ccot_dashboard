# modules/tab_rrt_outcomes.R
# Tab 3: RRT Outcomes — mirrors CCOT outcomes but indexed on RRT activation.
# Includes free-text NLP bucketing of activation_reason with stringr.

# ---- Activation-reason bucketing ---------------------------------------
# Lightweight pattern bucketing. Each row is checked against each pattern
# in order; the first match wins. Extend or replace with a proper NLP
# pipeline (e.g. quanteda or a trained classifier) when available.
RRT_REASON_BUCKETS <- tibble::tribble(
  ~bucket,             ~pattern,
  "Respiratory",       "respirator|breathing|hypox|desat|apnea|stridor|wheez",
  "Cardiovascular",    "hypotens|perfus|bradyc|tachyc|arrhyth|cardiac",
  "Neurologic",        "seizure|mental status|altered|unresponsive|neuro",
  "Sepsis/Infection",  "sepsis|infect|fever",
  "Family/Nurse concern", "family concern|nurse concern|concern about"
)

categorize_rrt_reason <- function(text) {
  txt <- tolower(ifelse(is.na(text), "", text))
  out <- rep("Other", length(txt))
  for (i in seq_len(nrow(RRT_REASON_BUCKETS))) {
    hit <- stringr::str_detect(txt, RRT_REASON_BUCKETS$pattern[i])
    out[hit & out == "Other"] <- RRT_REASON_BUCKETS$bucket[i]
  }
  factor(out, levels = c(RRT_REASON_BUCKETS$bucket, "Other"))
}

rrtOutcomesUI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "RRT Outcomes",
    icon  = bsicons::bs_icon("exclamation-triangle"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Filters",
        selectInput(ns("unit"), "Unit",
                    choices = c("All", FLOOR_UNITS), selected = "All"),
        checkboxGroupInput(ns("shift"), "Time of day (activation)",
                           choices = SHIFT_BUCKETS$shift,
                           selected = SHIFT_BUCKETS$shift),
        checkboxGroupInput(ns("dow"), "Day of week",
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
          bslib::card_header("Activation reason — NLP buckets"),
          plotly::plotlyOutput(ns("reason"), height = "320px")
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Disposition after RRT"),
          plotly::plotlyOutput(ns("disposition"), height = "320px")
        ),
        bslib::card(
          bslib::card_header("RRT hour-of-day — emergency vs not"),
          plotly::plotlyOutput(ns("hour_outcome"), height = "320px")
        )
      ),
      bslib::card(
        bslib::card_header("RRT activations (filtered)"),
        DT::DTOutput(ns("table"))
      )
    )
  )
}

rrtOutcomesServer <- function(id, date_range) {
  moduleServer(id, function(input, output, session) {

    rrt_with_bucket <- rrt_cases |>
      dplyr::mutate(reason_bucket = categorize_rrt_reason(activation_reason))

    cases_f <- reactive({
      df <- filter_by_date(rrt_with_bucket, "T0", date_range())
      if (!is.null(input$unit) && input$unit != "All") {
        df <- df[df$unit == input$unit, , drop = FALSE]
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
        kpi_box("RRT activations", n, NULL, icon = "exclamation-triangle",
                theme_color = "warning"),
        kpi_box("Transfer ≤2h",  windowed_rate(2),  NULL, icon = "stopwatch"),
        kpi_box("Transfer ≤12h", windowed_rate(12), NULL, icon = "stopwatch"),
        kpi_box("Transfer ≤24h", windowed_rate(24), NULL, icon = "stopwatch"),
        kpi_box("Transfer ≤48h", windowed_rate(48), NULL, icon = "stopwatch"),
        kpi_box("Emergency transfer", emerg_rate,
                "Intubation or vasopressor within 72h",
                icon = "heart-pulse", theme_color = "danger")
      )
    })

    output$funnel <- plotly::renderPlotly({
      df <- cases_f()
      validate(need(nrow(df) > 0, "No RRT activations match filters."))
      rates <- purrr::map_dfr(TRANSFER_WINDOWS_HOURS, function(h) {
        tibble::tibble(
          window_h = h,
          rate = mean(!is.na(df$hours_to_transfer) &
                        df$hours_to_transfer >= 0 &
                        df$hours_to_transfer <= h)
        )
      })
      p <- ggplot(rates, aes(factor(window_h), rate)) +
        geom_col(fill = "#d62728") +
        geom_text(aes(label = scales::percent(rate, 0.1)),
                  vjust = -0.4, size = 3.5) +
        scale_y_continuous(labels = scales::percent_format(1),
                           expand = expansion(mult = c(0, 0.15))) +
        labs(x = "Hours after activation", y = "Cumulative transfer rate") +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y")))
    })

    output$reason <- plotly::renderPlotly({
      df <- cases_f()
      validate(need(nrow(df) > 0, "No data."))
      sm <- df |>
        dplyr::count(reason_bucket, name = "n") |>
        dplyr::arrange(dplyr::desc(n))
      p <- ggplot(sm, aes(forcats::fct_reorder(reason_bucket, n), n,
                          fill = reason_bucket)) +
        geom_col() +
        coord_flip() +
        labs(x = NULL, y = "Activations") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y")))
    })

    output$disposition <- plotly::renderPlotly({
      df <- cases_f()
      validate(need(nrow(df) > 0, "No data."))
      sm <- df |> dplyr::count(disposition, name = "n")
      p <- ggplot(sm, aes(forcats::fct_reorder(disposition, n), n,
                          fill = disposition)) +
        geom_col() +
        geom_text(aes(label = n), vjust = -0.3, size = 3.3) +
        labs(x = NULL, y = "Activations") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y")))
    })

    output$hour_outcome <- plotly::renderPlotly({
      df <- cases_f()
      validate(need(nrow(df) > 0, "No data."))
      sm <- df |>
        dplyr::mutate(hour = floor(T0_hour),
                      group = ifelse(emergency_transfer,
                                     "Emergency transfer",
                                     "No / non-emergent")) |>
        dplyr::count(hour, group, name = "n")
      p <- ggplot(sm, aes(hour, n, fill = group)) +
        geom_col() +
        scale_fill_manual(values = c("Emergency transfer" = "#d62728",
                                     "No / non-emergent"  = "#2ca02c")) +
        scale_x_continuous(breaks = seq(0, 23, by = 3)) +
        labs(x = "Hour of activation", y = "Activations", fill = NULL) +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "fill")))
    })

    output$table <- DT::renderDT({
      df <- cases_f() |>
        dplyr::transmute(
          pat_enc_csn, unit, T0, T0_shift, T0_dow,
          reason_bucket, disposition,
          hours_to_transfer = round(hours_to_transfer, 1),
          emergency_transfer,
          activation_reason
        )
      DT::datatable(df, rownames = FALSE, filter = "top",
                    options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}
