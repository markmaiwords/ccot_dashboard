# app.R
# Entry point. Sources global.R (which loads libraries, simulated data,
# and all module files), then ui.R and server.R, and starts the app.

source("global.R", local = FALSE)
source("ui.R",     local = FALSE)
source("server.R", local = FALSE)

shinyApp(ui = ui, server = server)
