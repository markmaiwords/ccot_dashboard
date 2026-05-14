# modules/tab_data_quality.R
# Tab 6: Data Quality — note parse success, missingness, PEWS completeness.

dataQualityUI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Data Quality",
    icon  = bsicons::bs_icon("shield-check"),
    bslib::layout_columns(
      fill = FALSE,
      uiOutput(ns("kpis"))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Note parse success — weekly trend"),
        plotly::plotlyOutput(ns("parse_trend"), height = "340px")
      ),
      bslib::card(
        bslib::card_header("Missing-data summary across joined tables"),
        plotly::plotlyOutput(ns("missing"), height = "340px")
      )
    ),
    bslib::card(
      bslib::card_header("Failed note parses (manual review queue)"),
      DT::DTOutput(ns("dq_failed_table"))
    )
  )
}

dataQualityServer <- function(id, date_range) {
  moduleServer(id, function(input, output, session) {

    ccot_f <- reactive({
      filter_by_date(ccot_assessments, "first_assessment_datetime",
                     date_range())
    })

    output$kpis <- renderUI({
      df <- ccot_f()
      n  <- nrow(df)
      parse_pct <- if (n) mean(df$note_parse_success) else NA

      # PEWS completeness = encounters with >= 3 flowsheet rows within 24h
      # of T0 (enough to estimate a pre-event trajectory).
      ccot_in_range <- filter_by_date(ccot_cases, "T0", date_range())
      pf <- flowsheet_rows |>
        dplyr::inner_join(
          ccot_in_range |> dplyr::select(pat_enc_csn, T0),
          by = "pat_enc_csn"
        ) |>
        dplyr::mutate(rel_hour = hours_between(flowsheet_datetime, T0)) |>
        dplyr::filter(rel_hour >= -24, rel_hour <= 0) |>
        dplyr::count(pat_enc_csn, name = "n_rows")
      complete <- sum(pf$n_rows >= 3)
      pews_pct <- if (nrow(ccot_in_range))
        complete / nrow(ccot_in_range) else NA

      bslib::layout_columns(
        kpi_box("CCOT notes in range", n, NULL, icon = "journal-text"),
        kpi_box("Parse success",
                if (is.na(parse_pct)) "—" else
                  scales::percent(parse_pct, accuracy = 0.1),
                "note_parse_success = TRUE",
                icon = "check2-circle", theme_color = "success"),
        kpi_box("Failed parses",
                if (n) sum(!df$note_parse_success) else 0,
                "Queued for manual review",
                icon = "x-octagon", theme_color = "danger"),
        kpi_box("PEWS completeness",
                if (is.na(pews_pct)) "—" else
                  scales::percent(pews_pct, accuracy = 0.1),
                "≥3 flowsheet rows within 24h pre-T0",
                icon = "graph-up")
      )
    })

    output$parse_trend <- plotly::renderPlotly({
      df <- ccot_f()
      validate(need(nrow(df) > 0, "No data."))
      sm <- df |>
        dplyr::mutate(week = lubridate::floor_date(first_assessment_datetime,
                                                  "week", week_start = 1)) |>
        dplyr::group_by(week) |>
        dplyr::summarise(
          parse_rate = mean(note_parse_success),
          n = dplyr::n(), .groups = "drop"
        )
      p <- ggplot(sm, aes(week, parse_rate)) +
        geom_line(linewidth = 0.9, color = "#1f77b4") +
        geom_point(aes(size = n), color = "#1f77b4", alpha = 0.7) +
        scale_y_continuous(labels = scales::percent_format(1),
                           limits = c(0, 1)) +
        labs(x = NULL, y = "Parse success rate", size = "Notes / week") +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "size")))
    })

    output$missing <- plotly::renderPlotly({
      tables <- list(
        ccot_assessments = ccot_assessments,
        deterioration_fact = deterioration_fact,
        rrt_fact = rrt_fact,
        transfers = transfers,
        flowsheet_rows = flowsheet_rows,
        charges = charges,
        nomad_survey = nomad_survey
      )
      sm <- purrr::imap_dfr(tables, function(df, tbl) {
        purrr::imap_dfr(df, function(col, nm) {
          tibble::tibble(
            table = tbl, column = nm,
            pct_missing = mean(is.na(col))
          )
        })
      }) |> dplyr::filter(pct_missing > 0)
      validate(need(nrow(sm) > 0, "No missing values detected — good!"))

      p <- ggplot(sm, aes(forcats::fct_reorder(
                    paste0(table, ".", column), pct_missing),
                  pct_missing, fill = table)) +
        geom_col() + coord_flip() +
        scale_y_continuous(labels = scales::percent_format(1)) +
        labs(x = NULL, y = "% missing", fill = NULL) +
        theme_minimal(base_size = 11)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y")))
    })

    output$dq_failed_table <- DT::renderDT({
      df <- ccot_f() |>
        dplyr::filter(!note_parse_success) |>
        dplyr::select(pat_enc_csn, encounter_id,
                      first_assessment_datetime, recommendation_text)
      DT::datatable(df, rownames = FALSE, filter = "top",
                    options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}
