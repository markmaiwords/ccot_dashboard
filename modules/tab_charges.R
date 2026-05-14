# modules/tab_charges.R
# Tab 4: Charges & Finance — pro-fee charges and billing capture rate.

chargesUI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Charges & Finance",
    icon  = bsicons::bs_icon("currency-dollar"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Filters",
        selectInput(ns("provider"), "Provider",
                    choices = NULL, multiple = TRUE),
        selectInput(ns("unit"), "Unit",
                    choices = c("All", FLOOR_UNITS), selected = "All"),
        selectInput(ns("cpt"), "CPT / E&M code",
                    choices = NULL, multiple = TRUE)
      ),
      uiOutput(ns("kpis")),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          bslib::card_header("Monthly charges trend"),
          plotly::plotlyOutput(ns("trend"), height = "320px")
        ),
        bslib::card(
          bslib::card_header("Charges by CPT/E&M code"),
          plotly::plotlyOutput(ns("by_cpt"), height = "320px")
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Charges by provider"),
          plotly::plotlyOutput(ns("by_provider"), height = "320px")
        ),
        bslib::card(
          bslib::card_header("Charges by unit"),
          plotly::plotlyOutput(ns("by_unit"), height = "320px")
        )
      ),
      bslib::card(
        bslib::card_header("Charges (filtered)"),
        DT::DTOutput(ns("table"))
      )
    )
  )
}

chargesServer <- function(id, date_range) {
  moduleServer(id, function(input, output, session) {

    observe({
      updateSelectInput(session, "provider",
                        choices = sort(unique(charges$provider)))
      updateSelectInput(session, "cpt",
                        choices = sort(unique(charges$cpt_code)))
    })

    charges_f <- reactive({
      df <- filter_by_date(charges, "charge_date", date_range())
      if (length(input$provider)) df <- df[df$provider %in% input$provider, ]
      if (!is.null(input$unit) && input$unit != "All")
        df <- df[df$unit == input$unit, ]
      if (length(input$cpt)) df <- df[df$cpt_code %in% input$cpt, ]
      df
    })

    output$kpis <- renderUI({
      df_ch <- charges_f()
      total <- sum(df_ch$charge_amount)
      n_billed <- dplyr::n_distinct(df_ch$pat_enc_csn)

      # Billing capture rate is anchored to CCOT encounters in the global
      # date window, regardless of provider/CPT filters above (denominator
      # is "all CCOT encounters we saw").
      ccot_in_range <- filter_by_date(ccot_cases, "T0", date_range())
      n_ccot <- nrow(ccot_in_range)
      n_billed_ccot <- dplyr::n_distinct(
        charges$pat_enc_csn[charges$pat_enc_csn %in% ccot_in_range$pat_enc_csn &
                              as.Date(charges$charge_date) >= date_range()[1] &
                              as.Date(charges$charge_date) <= date_range()[2]]
      )
      cap_rate <- if (n_ccot) n_billed_ccot / n_ccot else NA

      bslib::layout_columns(
        fill = FALSE,
        kpi_box("Total charges", usd(total), NULL,
                icon = "currency-dollar", theme_color = "success"),
        kpi_box("Billed encounters", n_billed,
                "Distinct CSNs with at least one charge",
                icon = "receipt"),
        kpi_box("Billing capture rate",
                if (is.na(cap_rate)) "—" else
                  scales::percent(cap_rate, accuracy = 0.1),
                glue::glue("{n_billed_ccot} billed / {n_ccot} CCOT encounters"),
                icon = "check2-square"),
        kpi_box("Average charge", usd(if (nrow(df_ch))
          mean(df_ch$charge_amount) else 0),
          "Per charge line", icon = "calculator")
      )
    })

    output$trend <- plotly::renderPlotly({
      df <- charges_f()
      validate(need(nrow(df) > 0, "No charges in selection."))
      sm <- df |>
        dplyr::mutate(month = lubridate::floor_date(charge_date, "month")) |>
        dplyr::group_by(month) |>
        dplyr::summarise(total = sum(charge_amount), .groups = "drop")
      p <- ggplot(sm, aes(month, total)) +
        geom_line(linewidth = 0.9, color = "#2ca02c") +
        geom_point(size = 1.8, color = "#2ca02c") +
        scale_y_continuous(labels = scales::label_dollar()) +
        labs(x = NULL, y = "Total monthly charges") +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y")))
    })

    output$by_cpt <- plotly::renderPlotly({
      df <- charges_f()
      validate(need(nrow(df) > 0, "No charges in selection."))
      sm <- df |>
        dplyr::group_by(cpt_code, cpt_descr) |>
        dplyr::summarise(total = sum(charge_amount),
                         n = dplyr::n(), .groups = "drop")
      p <- ggplot(sm, aes(forcats::fct_reorder(cpt_code, total), total,
                          fill = cpt_code,
                          text = paste0(cpt_code, " — ", cpt_descr,
                                        "<br>", usd(total), " (n=", n, ")"))) +
        geom_col() +
        coord_flip() +
        scale_y_continuous(labels = scales::label_dollar()) +
        labs(x = NULL, y = "Total charges") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      plotly_config(plotly::ggplotly(p, tooltip = "text"))
    })

    output$by_provider <- plotly::renderPlotly({
      df <- charges_f()
      validate(need(nrow(df) > 0, "No charges in selection."))
      sm <- df |>
        dplyr::group_by(provider) |>
        dplyr::summarise(total = sum(charge_amount), .groups = "drop")
      p <- ggplot(sm, aes(forcats::fct_reorder(provider, total), total,
                          fill = provider)) +
        geom_col() + coord_flip() +
        scale_y_continuous(labels = scales::label_dollar()) +
        labs(x = NULL, y = "Charges") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y")))
    })

    output$by_unit <- plotly::renderPlotly({
      df <- charges_f()
      validate(need(nrow(df) > 0, "No charges in selection."))
      sm <- df |>
        dplyr::group_by(unit) |>
        dplyr::summarise(total = sum(charge_amount), .groups = "drop")
      p <- ggplot(sm, aes(forcats::fct_reorder(unit, total), total,
                          fill = unit)) +
        geom_col() + coord_flip() +
        scale_y_continuous(labels = scales::label_dollar()) +
        labs(x = NULL, y = "Charges") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y")))
    })

    output$table <- DT::renderDT({
      df <- charges_f()
      DT::datatable(df, rownames = FALSE, filter = "top",
                    options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatCurrency("charge_amount")
    })
  })
}
