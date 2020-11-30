
# Packages
library(tidyverse); library(sf); library(urbnmapr)
library(leaflet); library(leaflet.extras)

# Functions
map_data <- function(data) {
  
  data %>% 
    left_join(states_sf, ., by = c("state_name", "state_abbv")) %>% 
    st_transform(4326)
}

create_palette <- function(data, selected) {
  
  colorQuantile(palette = input$access_map_palette, 
                domain = pull(data, !!sym(selected)),
                n = 4)
}

render_map <- function(data, selected, palette) {
  
  title <- selected
  
  map <- data %>%
    mutate(map_focus = !!sym(selected)) %>% 
    leaflet() %>%
    clearShapes() %>%
    addResetMapButton() %>% 
    addPolygons(stroke = TRUE, color = "#444444", weight = 1, smoothFactor = 0, 
                fillColor = ~palette(map_focus),
                fillOpacity = 1,
                popup = ~paste(state_name, " (", state_abbv, ")", "<br>",
                               percent(map_focus,
                                       scale = 1, accuracy = 0.1)),
                layerId = ~state_name) %>% 
    addLegend(position = "topright", pal = palette,
              values = ~map_focus,
              labFormat =  function(type, cuts, p) {
                n = length(cuts)
                paste0(as.numeric(cuts)[-n], " &ndash; ", as.numeric(cuts)[-1])
              },
              title = title)
  
  map
  
}