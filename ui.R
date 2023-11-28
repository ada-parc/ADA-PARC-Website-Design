# ui.R
library(shiny)
library(shinythemes)
library(here)

fluidPage(
  navbarPage(
    "ADA-PARC",
    tabPanel("Home", source(here::here("ui", "home_ui.R"))),
    tabPanel("National Data", source(here::here("ui", "national_ui.R"))),
    tabPanel("City Data", source(here::here("ui", "city_ui.R"))),
    tabPanel("Accessibility", source(here::here("ui", "accessibility_ui.R")))
  )
)
