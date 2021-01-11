library(tidyverse); library(readxl); library(openxlsx); library(summarytools); library(janitor); 
library(sf); library(urbnmapr); library(geofacet); library(leaflet); library(leaflet.extras);
library(htmltools); library(shiny); library(shinyWidgets); library(flexdashboard); 
library(colourpicker); library(DT); library(highcharter);
library(scales); library(hrbrthemes); library(ggthemes)

# Must install urbnmapr via GitHUb if is not already installed
# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnmapr")

# Set database path
db_path <- "February 2020 update 2018 ACS/2.20.20DatabaseACS2018.final.xlsx"

# Demographics
demographics <- read_excel(path = db_path,
                           sheet = "Demographics", 
                           .name_repair = make_clean_names,
                           skip = 2) %>% 
  remove_empty(c("rows", "cols")) %>%
  rename("abbrev" = state_abbreviation) %>%
  mutate(across(-c(state:city), as.numeric))

# City dataa
metro_demo_data <- demographics %>% 
  filter(is.na(city) == FALSE) %>% 
  mutate(metro_state = paste0(city, ", ", abbrev)) %>% 
  select(metro_state,
         pwd, total_population, contains("percent")) %>%  # This is cleaner than listing out every variable
  mutate_if(is.numeric, round, 1)


# Data table
# DT::renderDataTable({
  
  # Table
  # DT::datatable(
  #   demo_metro_data %>% 
  #     rename("percent_pwd" = percent_of_total_population_with_a_disability,
  #            "total_pop" = total_population) %>% 
  #     mutate(percent_pwd = percent(percent_pwd, scale = 1, accuracy = 0.1),
  #            pwd = comma(pwd),
  #            total_pop = comma(total_pop)),
  #   options(scrollX = "200px"))
  # 
# })

# demo_gen_data %>%
#   leaflet() %>%
### Testing out the selectize - ggplot conversio in the by Metro tab  
  metro_select <- c("Albuquerque, NM", "Anchorage, AK")
  var_select <- c("pwd", "total_population","percent_people_18_64_with_a_disability")
  
  demo_metro_data %>% 
    mutate(full = paste0(city, ", ", state_abbv)) %>%
    select(full, var_select) %>%
    rename("City" = full) %>%
    filter(City %in% metro_select) %>%
    ggplot(aes(x = City, y = var_select[1])) +
    geom_col()
  
  ### Comparison example, not final version. Use as reference
  
  metro_demo_data %>% 
    filter(metro_state %in% metro_select) %>% 
    select("City" = metro_state, everything()) %>%
    pivot_longer(
      cols = -"City",
      names_to = "Variable"
    ) %>%
    pivot_wider(
      id_cols = Variable,
      names_from = City
    )
    

metro_demo_data %>% filter(metro_state != "United States, USA") %>%
  ggplot(aes(x = total_population, y = percent_of_total_population_with_a_disability)) + 
  geom_jitter() 

metro_demo_data %>% filter(metro_state != "United States, USA") %>%
  plotly::plot_ly(x = ~total_population, y = ~percent_of_total_population_with_a_disability, text = ~paste(metro_state)) %>% 
  plotly::add_markers() %>% 
  plotly::layout(xaxis = list(type = "log"))

###-------------------------------------------------------------------------###

### Tile mapping to replace the leaflet application in the national tab
# Libraries required
library(tidyverse)
library(geofacet)

# Set database path
db_path <- "February 2020 update 2018 ACS/2.20.20DatabaseACS2018.final.xlsx"

# Demographics
demographics <- read_excel(path = db_path,
                           sheet = "Demographics", 
                           .name_repair = make_clean_names,
                           skip = 2) %>% 
  remove_empty(c("rows", "cols")) %>%
  rename("abbrev" = state_abbreviation) %>%
  mutate(across(-c(state:city), as.numeric))

# Data for this section
demo_gen_data <- demographics %>% 
  filter(is.na(city)) %>% 
  select("state_name" = state, "state_abbv" = abbrev, 
         percent_of_total_population_with_a_disability, pwd, total_population) %>% 
  mutate(percent_of_total_population_with_a_disability =
           round(percent_of_total_population_with_a_disability, 1))

# Tile mapping using geofacet
demo_gen_data %>%
  ggplot(aes(x = 1, y = 1, # A tile map without x or y axis changes will fill out the tile for the state
             fill = -percent_of_total_population_with_a_disability)) + # Selected variable
  geom_tile() + # Imports x and y values
  labs(x = "", y = "") +
  geom_text(aes(label = paste0(as.character(percent_of_total_population_with_a_disability), "%")), 
            color = "white") + # Adds percentage to the center of the tile
  facet_geo(facets = ~ state_abbv, grid = "us_state_with_DC_PR_grid2") + 
  theme(plot.background = element_rect(colour = "white"), # Removes all of the grid elements that we don't need
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.spacing = unit(0L, "pt"),
        legend.position = "none",
        strip.text.x = element_text(size = 9L))


### From Kyle Walker's Neighborhood Diversity Index dashboard
### This can work as a template to implement an interactive national map
library(plotly)

# Here, we draw the diversity gradient with ggplotly
output$scatter <- renderPlotly({
  
  
  key <- metro()$tractid # This will uniquely identify tracts for Plotly
  
  p1a <- ggplot(metro()@data) + 
    geom_point(alpha = 0.4, aes(Distance, Score, key = key)) + 
    theme_minimal(base_size = 14) + 
    stat_smooth(aes(Distance, Score), 
                color = 'red', method = 'loess', span = input$span, se = FALSE) + 
    xlab('Distance from city hall (miles)') + ylab('') 
  
  g <- ggplotly(p1a, source = 'source') %>% 
    layout(dragmode = 'lasso', 
           yaxis = list(title = 'Diversity score'), 
           margin = list(l = 100), 
           font = list(family = 'Open Sans', size = 16))
  
  # Need to manually set the hoverinfo to avoid the key appearing in it
  build <- plotly_build(g)
  
  build$data[[1]]$text <- paste0('Distance: ', as.character(round(metro()$Distance, 2)), '<br>', 
                                 'Score: ', as.character(round(metro()$Score, 2))) 
  build
  
})  

# User can click on an individual point (or in this case state)
output$click <- renderPrint({
  d <- event_data("plotly_click")
  if (!is.null(d)) d
})

plotlyOutput('scatter', width = "80%")

### --------------------------------------------------------------------------- ###
### National Tab Redux
###

### --------------------
### Pull data
###


### Demographics
# S1810
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_S1810",
                                 database_name = "ADA-PARC")
metro_snapshot_state_demographic <- temp_mongo_conn$find()

rm(temp_mongo_conn)

### Community Living
# S2601A
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_S2601A",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp1 <- temp_mongo_conn$find()

# S2602
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_S2602",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp2 <- temp_mongo_conn$find()

# S1810
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_S1810",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp3 <- temp_mongo_conn$find()

# Join together
metro_snapshot_state_living <- list(metro_snapshot_state_temp1,
                                  metro_snapshot_state_temp2,
                                  metro_snapshot_state_temp3) %>% 
  reduce(left_join, by = c("GEOID", "NAME"))

rm(temp_mongo_conn, metro_snapshot_state_temp1, metro_snapshot_state_temp2, metro_snapshot_state_temp3)

### Community Participation
# B18135
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_B18135",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp1 <- temp_mongo_conn$find()

# S1811
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_S1811",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp2 <- temp_mongo_conn$find()

# S1810
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_S1810",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp3 <- temp_mongo_conn$find()

# Join together
metro_snapshot_state_participation <- list(metro_snapshot_state_temp1,
                                    metro_snapshot_state_temp2,
                                    metro_snapshot_state_temp3) %>% 
  reduce(left_join, by = c("GEOID", "NAME"))

rm(temp_mongo_conn, metro_snapshot_state_temp1, metro_snapshot_state_temp2, metro_snapshot_state_temp3)

### Work/Economic
# B18140
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_B18140",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp1 <- temp_mongo_conn$find()

# C18120
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_C18120",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp2 <- temp_mongo_conn$find()

# C18121
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_C18121",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp3 <- temp_mongo_conn$find()

# C18130
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_C18130",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp4 <- temp_mongo_conn$find()

# S1811
temp_mongo_conn <- mongo_connect(collection_name = "acs_state_S1811",
                                 database_name = "ADA-PARC")
metro_snapshot_state_temp5 <- temp_mongo_conn$find()

# Join together
metro_snapshot_state_economic <- list(metro_snapshot_state_temp1,
                                           metro_snapshot_state_temp2,
                                           metro_snapshot_state_temp3,
                                           metro_snapshot_state_temp4,
                                           metro_snapshot_state_temp5) %>% 
  reduce(left_join, by = c("GEOID", "NAME"))

rm(temp_mongo_conn, metro_snapshot_state_temp1, metro_snapshot_state_temp2, metro_snapshot_state_temp3, metro_snapshot_state_temp4, metro_snapshot_state_temp5)
