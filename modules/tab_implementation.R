# modules/tab_implementation.R
# Tab 5: Implementation (NoMAD) — two respondent tracks shown separately.

implementationUI <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Implementation",
    icon  = bsicons::bs_icon("clipboard-check"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Filters",
        checkboxGroupInput(ns("role"), "Respondent role",
                           choices = c("CCOT team", "Floor provider"),
                           selected = c("CCOT team", "Floor provider")),
        checkboxGroupInput(ns("subscale"), "Subscale",
                           choices = NOMAD_SUBSCALES,
                           selected = NOMAD_SUBSCALES)
      ),
      bslib::card(
        bslib::card_header("CCOT team — weekly subscale trends"),
        plotly::plotlyOutput(ns("ccot_trend"), height = "340px")
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Floor providers — snapshot (sparse)"),
          plotly::plotlyOutput(ns("floor_dot"), height = "340px")
        ),
        bslib::card(
          bslib::card_header("Subscale score distributions"),
          plotly::plotlyOutput(ns("violin"), height = "340px")
        )
      ),
      bslib::card(
        bslib::card_header("Free-text responses"),
        DT::DTOutput(ns("free_text"))
      )
    )
  )
}

implementationServer <- function(id, date_range) {
  moduleServer(id, function(input, output, session) {

    survey_f <- reactive({
      df <- filter_by_date(nomad_survey, "survey_date", date_range())
      if (length(input$role)) df <- df[df$respondent_role %in% input$role, ]
      df
    })

    long <- reactive({
      survey_f() |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(NOMAD_SUBSCALES),
          names_to = "subscale", values_to = "score"
        ) |>
        dplyr::filter(subscale %in% input$subscale)
    })

    output$ccot_trend <- plotly::renderPlotly({
      df <- long() |> dplyr::filter(respondent_role == "CCOT team")
      validate(need(nrow(df) > 0, "No CCOT team responses in range."))
      sm <- df |>
        dplyr::group_by(survey_date, subscale) |>
        dplyr::summarise(mean_score = mean(score),
                         se = stats::sd(score) / sqrt(dplyr::n()),
                         .groups = "drop")
      p <- ggplot(sm, aes(survey_date, mean_score, color = subscale)) +
        geom_ribbon(aes(ymin = mean_score - se, ymax = mean_score + se,
                        fill = subscale),
                    alpha = 0.15, color = NA) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.4) +
        coord_cartesian(ylim = c(1, 5)) +
        labs(x = NULL, y = "Mean score (1-5)", color = NULL, fill = NULL) +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "colour")))
    })

    output$floor_dot <- plotly::renderPlotly({
      df <- long() |> dplyr::filter(respondent_role == "Floor provider")
      validate(need(nrow(df) > 0, "No floor provider responses in range."))
      p <- ggplot(df, aes(survey_date, score, color = subscale)) +
        geom_jitter(width = 1.5, height = 0.05, alpha = 0.75, size = 2) +
        coord_cartesian(ylim = c(1, 5)) +
        labs(x = NULL, y = "Score (1-5)", color = NULL) +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "colour")))
    })

    output$violin <- plotly::renderPlotly({
      df <- long()
      validate(need(nrow(df) > 0, "No responses in selection."))
      p <- ggplot(df, aes(subscale, score, fill = respondent_role)) +
        geom_violin(alpha = 0.6, scale = "width", position =
                      position_dodge(width = 0.8)) +
        geom_boxplot(width = 0.12, alpha = 0.8,
                     position = position_dodge(width = 0.8)) +
        coord_cartesian(ylim = c(1, 5)) +
        labs(x = NULL, y = "Score (1-5)", fill = NULL) +
        theme_minimal(base_size = 12)
      plotly_config(plotly::ggplotly(p, tooltip = c("x", "y", "fill")))
    })

    output$free_text <- DT::renderDT({
      df <- survey_f() |>
        dplyr::filter(!is.na(free_text), nzchar(free_text)) |>
        dplyr::select(survey_date, respondent_role, respondent_id, free_text)
      DT::datatable(df, rownames = FALSE, filter = "top",
                    options = list(pageLength = 10))
    })
  })
}
