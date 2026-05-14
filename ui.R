# ui.R
# Top-level UI: bslib navbar with a global sidebar (date filter) and one
# nav_panel per module. The sidebar is positioned at the page level so it
# applies to every tab.

ui <- bslib::page_navbar(
  title = "CCOT / RRT Program Dashboard",
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1f4e79"
  ),
  sidebar = bslib::sidebar(
    title = "Global filters",
    width = 280,
    dateRangeInput(
      "date_range", "Date range (applies to all tabs)",
      start = PROGRAM_START_DATE,
      end   = PROGRAM_END_DATE,
      min   = PROGRAM_START_DATE,
      max   = PROGRAM_END_DATE
    ),
    hr(),
    helpText(
      "T0 = first CCOT assessment (CCOT cases) or RRT activation",
      " (RRT cases). Observation window = ", OBSERVATION_WINDOW_HOURS,
      "h. Emergency transfer = intubation OR vasopressor at PICU",
      " transfer within window."
    ),
    hr(),
    helpText(
      tags$small(
        "Mock data — replace data/simulate_data.R with real ETL pulls",
        " before clinical use."
      )
    )
  ),
  overviewUI("overview"),
  ccotOutcomesUI("ccot"),
  rrtOutcomesUI("rrt"),
  chargesUI("charges"),
  implementationUI("implementation"),
  dataQualityUI("dq")
)
