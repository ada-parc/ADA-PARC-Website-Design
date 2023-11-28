# server.R
source(here::here("server", "home_server.R"))
source(here::here("server", "national_server.R"))
source(here::here("server", "city_server.R"))
source(here::here("server", "accessibility_server.R"))

server <- function(input, output, session) {
  callModule(home_server, "home")
  callModule(national_server, "national")
  callModule(city_server, "city")
  callModule(accessibility_server, "accessibility")
}