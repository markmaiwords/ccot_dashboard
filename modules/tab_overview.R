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
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header(
          "Patient pipeline — admitted to PICU transfer",
          tags$small(class = "text-muted ms-2",
                     "Sankey: stage counts derive from filtered data")
        ),
        plotly::plotlyOutput(ns("sankey"), height = "420px")
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

    output$sankey <- plotly::renderPlotly({
      # Filter encounters to the global date range using admit_datetime.
      enc_f <- filter_by_date(encounters, "admit_datetime", date_range())
      validate(need(nrow(enc_f) > 0,
                    "No admissions in selected date range."))

      # Stage 1: Admitted -> Watcher | Not flagged
      watcher_csns <- enc_f$pat_enc_csn[enc_f$is_watcher]
      n_admitted   <- nrow(enc_f)
      n_watcher    <- length(watcher_csns)
      n_notflagged <- n_admitted - n_watcher

      # Stage 2: among watchers -> CCOT eval | resolved without eval
      ccot_csns_in <- ccot_cases$pat_enc_csn[
        ccot_cases$pat_enc_csn %in% watcher_csns
      ]
      n_ccot       <- length(ccot_csns_in)
      n_no_ccot    <- n_watcher - n_ccot

      # Stage 3: among CCOT-evaluated watchers -> RRT | no RRT
      rrt_csns_in <- unique(rrt_fact$pat_enc_csn[
        rrt_fact$pat_enc_csn %in% ccot_csns_in
      ])
      n_rrt       <- length(rrt_csns_in)
      n_no_rrt    <- n_ccot - n_rrt

      # Stage 4: among RRT pipeline -> PICU transfer | stayed on floor
      transfers_in <- transfers[transfers$pat_enc_csn %in% rrt_csns_in, ]
      transfer_csns_in <- unique(transfers_in$pat_enc_csn)
      n_transfer  <- length(transfer_csns_in)
      n_no_transfer <- n_rrt - n_transfer

      # Stage 5: emergency vs routine
      tx <- transfers_in[!duplicated(transfers_in$pat_enc_csn), ]
      n_emerg   <- sum(tx$intubated_at_transfer | tx$vasopressor_at_transfer)
      n_routine <- n_transfer - n_emerg

      # Node order (0-indexed for plotly sankey).
      labels <- c(
        sprintf("Admitted (%d)", n_admitted),                # 0
        sprintf("Watcher list (%d)", n_watcher),             # 1
        sprintf("Not flagged (%d)", n_notflagged),           # 2
        sprintf("CCOT evaluated (%d)", n_ccot),              # 3
        sprintf("Resolved without CCOT eval (%d)", n_no_ccot), # 4
        sprintf("RRT activated (%d)", n_rrt),                # 5
        sprintf("De-escalated, no RRT (%d)", n_no_rrt),      # 6
        sprintf("PICU transfer (%d)", n_transfer),           # 7
        sprintf("Remained on floor (%d)", n_no_transfer),    # 8
        sprintf("Emergency transfer (%d)", n_emerg),         # 9
        sprintf("Routine transfer (%d)", n_routine)          # 10
      )

      node_colors <- c(
        "#1f4e79", # admitted
        "#3182bd", # watcher (on-program)
        "#bdbdbd", # not flagged (off-program terminal)
        "#2c7fb8", # CCOT eval
        "#bdbdbd", # no CCOT eval terminal
        "#fd8d3c", # RRT
        "#bdbdbd", # no RRT terminal
        "#de2d26", # PICU transfer
        "#bdbdbd", # remained on floor terminal
        "#a50f15", # emergency transfer
        "#fc9272"  # routine transfer
      )

      # Links: source, target, value.
      src <- c(0, 0, 1, 1, 3, 3, 5, 5, 7, 7)
      tgt <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val <- c(n_watcher, n_notflagged,
               n_ccot, n_no_ccot,
               n_rrt, n_no_rrt,
               n_transfer, n_no_transfer,
               n_emerg, n_routine)

      # Forward-flow links use a translucent version of the destination
      # color; dropouts use translucent gray.
      link_colors <- c(
        "rgba(49,130,189,0.45)",  # admitted -> watcher
        "rgba(189,189,189,0.35)", # admitted -> not flagged
        "rgba(44,127,184,0.45)",  # watcher -> CCOT
        "rgba(189,189,189,0.35)", # watcher -> no CCOT
        "rgba(253,141,60,0.45)",  # CCOT -> RRT
        "rgba(189,189,189,0.35)", # CCOT -> no RRT
        "rgba(222,45,38,0.45)",   # RRT -> transfer
        "rgba(189,189,189,0.35)", # RRT -> no transfer
        "rgba(165,15,21,0.55)",   # transfer -> emergency
        "rgba(252,146,114,0.55)"  # transfer -> routine
      )

      p <- plotly::plot_ly(
        type = "sankey",
        orientation = "h",
        arrangement = "snap",
        node = list(
          label = labels,
          color = node_colors,
          pad = 18,
          thickness = 20,
          line = list(color = "#ffffff", width = 0.5)
        ),
        link = list(
          source = src,
          target = tgt,
          value  = val,
          color  = link_colors
        )
      ) |>
        plotly::layout(
          font = list(family = "system-ui, Segoe UI, Roboto, sans-serif",
                      size = 12),
          margin = list(t = 10, l = 10, r = 10, b = 10)
        )
      plotly_config(p)
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
