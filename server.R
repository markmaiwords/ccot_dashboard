# server.R
# Wires the global date filter into each module server.

server <- function(input, output, session) {
  date_range <- reactive({ input$date_range })

  overviewServer("overview", date_range)
  ccotOutcomesServer("ccot", date_range)
  rrtOutcomesServer("rrt", date_range)
  chargesServer("charges", date_range)
  implementationServer("implementation", date_range)
  dataQualityServer("dq", date_range)
}
