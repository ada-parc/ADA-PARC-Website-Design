# app.R
library(shiny)

# Run the Shiny app
shinyApp(ui = source("ui.R"), server = source("server.R"))
