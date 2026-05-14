# modules/tab_overview.R
# Tab 1: Overview — high-level program KPIs, weekly volume, unit heatmap.

overviewUI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Overview",
    icon  = bsicons::bs_icon("speedometer2"),
    bslib::layout_columns(
      fill = FALSE,
      uiOutput(ns("kpis"))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Weekly volume — CCOT vs RRT"),
        plotly::plotlyOutput(ns("volume_trend"), height = "360px")
      ),
      bslib::card(
        bslib::card_header("Activity by unit and day of week"),
        plotly::plotlyOutput(ns("unit_heatmap"), height = "360px")
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Hour-of-day distribution — CCOT, RRT, transfers"),
        plotly::plotlyOutput(ns("hour_dist"), height = "320px")
      )
    )
  )
}

overviewServer <- function(id, date_range) {
  moduleServer(id, function(input, output, session) {

    ccot_f <- reactive({
      filter_by_date(ccot_cases, "T0", date_range())
    })
    rrt_f <- reactive({
      filter_by_date(rrt_cases, "T0", date_range())
    })
    tx_f <- reactive({
      filter_by_date(transfers, "transfer_datetime", date_range())
    })
    charges_f <- reactive({
      filter_by_date(charges, "charge_date", date_range())
    })

    output$kpis <- renderUI({
      n_ccot <- nrow(ccot_f())
      n_rrt  <- nrow(rrt_f())

      # Emergency-transfer rates within 72h.
      ccot_emerg <- ccot_outcomes |>
        dplyr::semi_join(ccot_f(), by = "pat_enc_csn")
      rrt_emerg  <- rrt_cases |>
        dplyr::semi_join(rrt_f(), by = c("pat_enc_csn", "T0"))

      ccot_rate <- if (n_ccot) mean(ccot_emerg$emergency_transfer) else NA
      rrt_rate  <- if (n_rrt)  mean(rrt_emerg$emergency_transfer)  else NA

      total_charges <- sum(charges_f()$charge_amount)

      active_days <- if (n_ccot) {
        as.integer(diff(range(as.Date(ccot_f()$T0)))) + 1
      } else 0

      bslib::layout_columns(
        kpi_box("CCOT encounters", n_ccot,
                "First assessment per encounter",
                icon = "clipboard-pulse"),
        kpi_box("RRT activations", n_rrt,
                "All activations in range",
                icon = "exclamation-triangle", theme_color = "warning"),
        kpi_box("Emergency transfer rate (CCOT)",
                scales::percent(ccot_rate, accuracy = 0.1),
                "Intubation or vasopressor within 72h",
                icon = "heart-pulse", theme_color = "danger"),
        kpi_box("Emergency transfer rate (RRT)",
                scales::percent(rrt_rate, accuracy = 0.1),
                "Intubation or vasopressor within 72h",
                icon = "heart-pulse", theme_color = "danger"),
        kpi_box("Total charges", usd(total_charges),
                "Pro-fee in range", icon = "currency-dollar",
                theme_color = "success"),
        kpi_box("Active days", active_days,
                "Days with at least one CCOT encounter",
                icon = "calendar3")
      )
    })

    output$volume_trend <- plotly::renderPlotly({
      ccot_wk <- ccot_f() |>
        dplyr::mutate(week = lubridate::floor_date(T0, "week",
                                                  week_start = 1)) |>
        dplyr::count(week, name = "n") |>
        dplyr::mutate(series = "CCOT")
      rrt_wk <- rrt_f() |>
        dplyr::mutate(week = lubridate::floor_date(T0, "week",
                                                  week_start = 1)) |>
        dplyr::count(week, name = "n") |>
        dplyr::mutate(series = "RRT")
      df <- dplyr::bind_rows(ccot_wk, rrt_wk)
      validate(need(nrow(df) > 0, "No encounters in selected date range."))

      p <- ggplot(df, aes(week, n, color = series)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.6) +
        scale_color_manual(values = c(CCOT = "#1f77b4", RRT = "#d62728")) +
        labs(x = NULL, y = "Encounters / week", color = NULL) +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "colour")))
    })

    output$unit_heatmap <- plotly::renderPlotly({
      df <- ccot_f() |>
        dplyr::count(unit, T0_dow, name = "n")
      validate(need(nrow(df) > 0, "No data."))

      p <- ggplot(df, aes(T0_dow, unit, fill = n)) +
        geom_tile(color = "white") +
        scale_fill_viridis_c() +
        labs(x = NULL, y = NULL, fill = "Cases") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 0))
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "fill")))
    })

    output$hour_dist <- plotly::renderPlotly({
      ccot_h <- tibble::tibble(hour = floor(ccot_f()$T0_hour),
                               series = "CCOT note")
      rrt_h  <- tibble::tibble(hour = floor(rrt_f()$T0_hour),
                               series = "RRT call")
      tx_h   <- tibble::tibble(hour = lubridate::hour(tx_f()$transfer_datetime),
                               series = "Transfer")
      df <- dplyr::bind_rows(ccot_h, rrt_h, tx_h) |>
        dplyr::count(series, hour, name = "n")
      validate(need(nrow(df) > 0, "No data."))

      p <- ggplot(df, aes(hour, n, color = series)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.3) +
        scale_x_continuous(breaks = seq(0, 23, by = 3)) +
        labs(x = "Hour of day", y = "Count", color = NULL) +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "colour")))
    })
  })
}
