# national_server.R
library(shiny)
library(dplyr)
library(ggplot2)
library(maps)

national_data <- as.data.frame(readRDS(here::here("data", "final", "demographics.Rds")) %>% 
  select(NAME, pop_total))

national_server <- function(input, output, session) {
  output$us_map <- renderPlot({
    # Load map data for the United States
    us <- map_data("usa")
    
    # Create a basic map of the United States
    ggplot() +
      geom_polygon(data = us, aes(x = long, y = lat, group = group), 
                   fill = "lightblue", color = "white") +
      coord_fixed(1.3) +  # Aspect ratio adjustment
      theme_void()  # Remove axis and gridlines
  })
}
  