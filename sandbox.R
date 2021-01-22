library(tidyverse); library(readxl); library(openxlsx); library(summarytools); library(janitor) 
library(sf); library(urbnmapr); library(geofacet); library(leaflet); library(leaflet.extras);
library(htmltools); library(shiny); library(shinyWidgets); library(flexdashboard)
library(colourpicker); library(DT); library(gghighlight)
library(knitr); library(kableExtra)
library(scales); library(hrbrthemes); library(ggthemes)
library(extrafont); library(plotly)
library(mongolite)

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
### Setup
###

source("secret.R", local = TRUE)

# Function for pulling data from database with table names as input
pull_mongo_data <- function(tables, geo) {
  for (t in seq_along(tables)) {
    print(paste("Connection to:", paste0("acs_", geo, "_", tables[t])))
    temp_mongo_conn <- mongo_connect(collection_name = paste0("acs_", geo, "_", tables[t]),
                                     database_name = "ADA-PARC")
    
    assign(paste0("temp_df", t), temp_mongo_conn$find())
    
    rm(temp_mongo_conn)
  }
  df <- reduce(mget(ls(pattern = "temp_df")), 
               left_join, by = c("GEOID", "NAME"))
}

# Function to render national tile map
render_tile_map <- function(data, selected, grid = "us_state_with_DC_PR_grid2") {
  
  # ### Unnecessary if selected is being passed as a string
  # # but we may want to pass the variable itself at some point
  # selected_varname <- deparse(substitute(!!sym(selected))) %>% gsub("^[^\\$]*\\$", "", .) 
  # ### Parsing the regex
  # # ^ anchor for beginning of string
  # # [^\\$]* matches everything excluding "$" char
  # # \\$ terminates match at "$" char
  # # example: deparse(substitute(national_demographic_readable$GEOID)) %>% gsub("^[^\\$]*\\$", "", .)
  # # outputs: "GEOID"

  # Provide appropriate variable label to national tile map labels
  if(grepl("pct", selected_varname)) {
    selected_label <- "%"
  } else {
    selected_label <- ""
  }
  
  data %>%
    ggplot(aes(x = 1, y = 1, # A tile map without x or y axis changes will fill out the tile for the state
               fill = !!sym(selected))) + # Selected variable
    geom_tile() + # Imports x and y values
    geom_text(aes(label = paste0(!!sym(selected), selected_label)),
              color = "white") + # Adds percentage to the center of the tile
    labs(x = "", y = "") +
    facet_geo(facets = ~ NAME, grid = grid) +
    theme(plot.background = element_rect(colour = "white"), # Removes all of the grid elements that we don't need
          panel.grid = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.spacing = unit(0L, "pt"),
          legend.position = "none",
          strip.text.x = element_text(size = 9L))
}

render_tile_map(national_demographic_readable, "pct_pwd")

### --------------------
### Pull data
###

### Demographics
tables <- c("S1810")
national_demographic <- pull_mongo_data(tables, "state")
rm(tables)

### Community Living
tables <- c("S2601A", "S2602", "S1810")
national_living <- pull_mongo_data(tables, "state")
rm(tables)

### Community Participation
tables <- c("B18135", "S1811", "S1810")
national_participation <- pull_mongo_data(tables, "state")
rm(tables)

### Work/Economic
tables <- c("B18135", "B18140", "C18120", "C18121", "C18130", "S1811", "B25091", "B25070")
national_economic <- pull_mongo_data(tables, "state")
rm(tables)

### ---------------
### Clean data
###

### Human readable tables for use in dashboard
national_demographic_readable <- national_demographic %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    NAME_ABBRV = state,
    
    ### Pop pwd
    pop_total = estimate_S1810_C01_001,
    pwd_total = estimate_S1810_C02_001,
    pct_pwd = pwd_total / pop_total,
    
    ### Age
    pop_18_64 = estimate_S1810_C01_015 + estimate_S1810_C01_016,
    pwd_18_64 = estimate_S1810_C02_015 + estimate_S1810_C02_016,
    pct_pwd_18_64 = pwd_18_64 / pop_18_64,
    pop_grtoeq_65 = estimate_S1810_C01_017 + estimate_S1810_C01_018,
    pwd_grtoeq_65 = estimate_S1810_C02_017 + estimate_S1810_C02_018,
    pct_pwd_grtoeq_65 = pwd_grtoeq_65 / pop_grtoeq_65,
    
    ### Race
    pwd_white = estimate_S1810_C02_004,
    pwd_afam = estimate_S1810_C02_005,
    pwd_hisp = estimate_S1810_C02_012,
    pwd_whit_nonhisp = estimate_S1810_C02_011,
    pwd_other = estimate_S1810_C01_006 + estimate_S1810_C01_007 + estimate_S1810_C01_008 + estimate_S1810_C01_009 + estimate_S1810_C01_010,
    pct_pwd_white = pwd_white / pwd_total,
    pct_pwd_afam = pwd_afam / pwd_total,
    pct_pwd_hisp = pwd_hisp / pwd_total,
    pct_pwd_white_nonhisp = pwd_whit_nonhisp / pwd_total,
    pct_pwd_other = pwd_other / pwd_total,
    
    ### Gender
    pop_female = estimate_S1810_C01_003,
    pwd_female = estimate_S1810_C02_003,
    pct_female_pwd = pwd_female / pop_female,
    pct_pwd_female = pwd_female / pwd_total,
    pop_male = estimate_S1810_C01_002,
    pwd_male = estimate_S1810_C02_002,
    pct_male_pwd = pwd_male / pop_male,
    pct_pwd_male = pwd_male / pwd_total,
    
    ### Type of disability
    pwd_hearing = estimate_S1810_C02_019,
    pct_pwd_hearing = pwd_hearing / pwd_total,
    pwd_vision = estimate_S1810_C02_029,
    pct_pwd_vision = pwd_vision / pwd_total,
    pwd_cognitive = estimate_S1810_C02_039,
    pct_pwd_cognitive = pwd_cognitive / pwd_total,
    pwd_ambulatory = estimate_S1810_C02_047,
    pct_pwd_ambulatory = pwd_ambulatory / pwd_total,
    pwd_selfcare = estimate_S1810_C02_055,
    pct_pwd_selfcare = pwd_selfcare / pwd_total,
    pwd_indliving = estimate_S1810_C02_063,
    pct_pwd_indliving = pwd_indliving / pwd_total
  ) %>%
  mutate(across(.cols = starts_with("pct"),.fns = ~ round(.x * 100, 2)))

national_living_readable <- national_living %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    NAME_ABBRV = state,
    
    ### Pop pwd
    pop_total = estimate_S2601A_C01_001,
    pct_pwd = estimate_S2601A_C01_047,
    pwd_total = round(pop_total * (pct_pwd/100), 0),
    
    ### Group quarters
    pop_grpquarters = estimate_S2601A_C02_001,
    pct_pwd_grpquarters = estimate_S2601A_C02_047,
    pct_grpquarters = pop_grpquarters / pop_total,
    pop_grpquarters_institution = estimate_S2601A_C03_001,
    pop_grpquarters_noninstitution = estimate_S2601A_C04_001,
    pct_pwd_grpquarters_institution = estimate_S2601A_C03_047,
    pwd_grpquarters_institution = round(pop_grpquarters * (pct_pwd_grpquarters_institution/100), 0),
    pct_pwd_grpquarters_noninstitution = estimate_S2601A_C04_047,
    pwd_grpquarters_noninstitution = round(pop_grpquarters * (pct_pwd_grpquarters_noninstitution/100), 0),
    
    # DatabaseUpdate Instruction.xlsx says below stats are calculated via formula from above vars?
    # pct_pwd_institution = , # Is this just pct_pwd_grpquarters_institution?
    # pct_pwd_home = , # Not sure what variables are used to calculate this
    # pct_pwd_grpquarters_other = , # Couldn't find this in lookup_var df
    
    ### Nursing homes
    pop_nursing = estimate_S2602_C04_001,
    pop_18_64 = estimate_S2602_C01_047,
    pct_pwd_18_64 = estimate_S2602_C01_048, 
    pwd_18_64 = round(pop_total * (pct_pwd_18_64 / 100), 0),
    pct_pop_nursing_18_64 = estimate_S2602_C04_006 + estimate_S2602_C04_007 + estimate_S2602_C04_008 + estimate_S2602_C04_009 + estimate_S2602_C04_010,
    pct_pwd_nursing_18_64 = estimate_S2602_C04_048,
    pwd_nursing_18_64 = round(pwd_total * (pct_pwd_nursing_18_64/100), 0) 
  ) %>%
  mutate(across(.cols = starts_with("pct"),.fns = ~ round(.x * 100, 2)))

national_participation_readable <- national_participation %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    NAME_ABBRV = state,
    
    ### Health Insurance
    pop_19_64 = estimate_B18135_013,
    
    pwd_19_64 = estimate_B18135_014,
    pwd_19_64_insured = estimate_B18135_015,
    pwd_19_64_insured_private = estimate_B18135_016,
    pwd_19_64_insured_public = estimate_B18135_017,
    pwd_19_64_uninsured = estimate_B18135_018,
    
    pwod_19_64 = estimate_B18135_019,
    pwod_19_64_insured = estimate_B18135_020,
    pwod_19_64_insured_private = estimate_B18135_021,
    pwod_19_64_insured_public = estimate_B18135_022,
    pwod_19_64_uninsured = estimate_B18135_023,
    
    pop_grtoeq_65 = estimate_B18135_024,
    
    pwd_grtoeq_65 = estimate_B18135_025,
    pwd_grtoeq_65_insured = estimate_B18135_026,
    pwd_grtoeq_65_insured_private = estimate_B18135_027,
    pwd_grtoeq_65_insured_public = estimate_B18135_028,
    pwd_grtoeq_65_uninsured = estimate_B18135_029,
    
    pwod_grtoeq_65 = estimate_B18135_030,
    pwod_grtoeq_65_insured = estimate_B18135_031,
    pwod_grtoeq_65_insured_private = estimate_B18135_032,
    pwod_grtoeq_65_insured_public = estimate_B18135_033,
    pwod_grtoeq_65_uninsured = estimate_B18135_034,
    
    pct_pwd_19_64_uninsured = pwd_19_64_uninsured / pwd_19_64,
    pct_pwod_19_64_uninsured = pwod_19_64_uninsured / pwod_19_64 ,
    pct_pwd_grtoeq_65_uninsured = pwd_grtoeq_65_uninsured / pwd_grtoeq_65,
    pct_pwod_groeq_65_uninsured = pwod_grtoeq_65_uninsured / pwod_grtoeq_65,
    pct_pwd_19_64_insured_private = pwd_19_64_insured_private / pwd_19_64,
    pct_pwod_19_64_insured_private = pwod_19_64_insured_private / pwod_19_64,
    pct_pwd_grtoeq_65_insured_private = pwd_grtoeq_65_insured_private / pwd_grtoeq_65,
    pct_pwod_grtoeq_65_insured_private = pwod_grtoeq_65_insured_private / pwod_grtoeq_65,
    
    ### Medicare/Medicaid
    pop_total = estimate_S1810_C01_001,
    pwd_total = estimate_S1810_C02_001,
    pwod = pop_total - pwd_total,
    
    ### Transit Usage
    pct_pwd_commute_public = estimate_S1811_C02_035,
    pct_pwod_commute_public = estimate_S1811_C03_035,
    pct_pwd_commute_car_alone = estimate_S1811_C02_033,
    pct_pwod_commute_car_alone = estimate_S1811_C03_033,
    
    ### Educational Attainment
    pct_pwd_lessthan_highschool = estimate_S1811_C02_040,
    pct_pwod_lessthan_highschool = estimate_S1811_C03_040,
    pct_pwd_highschoolequiv = estimate_S1811_C02_041,
    pct_pwod_highschoolequiv = estimate_S1811_C03_041,
    pct_pwd_degree_aa = estimate_S1811_C02_042,
    pct_pwod_degree_aa = estimate_S1811_C03_042, 
    pct_pwd_degree_grtoeq_ba = estimate_S1811_C02_043,
    pct_pwod_degree_grtoeq_ba = estimate_S1811_C03_043
  ) %>%
  mutate(across(.cols = starts_with("pct"),.fns = ~ round(.x * 100, 2)))

national_economic_readable <- national_economic %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    NAME_ABBRV = state.x,
    
    ### Employment Status
    pop_total = estimate_C18120_001,
    pop_19_64 = estimate_B18135_013, # Not the same as the instructions spreadsheet; used this instead to keep calculations in same universe
    pwd_19_64 = estimate_B18135_014,
    pwod_19_64 = pop_19_64 - pwd_19_64,
    pwd_employed = estimate_C18120_004,
    pwod_employed = estimate_C18120_005,
    pwd_unemployed = estimate_C18120_007,
    pwod_unemployed = estimate_C18120_008,
    pwd_notlabor = estimate_C18120_010,
    pwod_notlabor = estimate_C18120_011,
    
    ### Poverty Status
    pop_18_64 = estimate_C18130_009,
    pwd_18_64 = estimate_C18130_010,
    pwod_18_64 = estimate_C18130_013,
    pwd_below_poverty = estimate_C18130_011,
    pwd_atorbelow_poverty = estimate_C18130_012,
    pwod_below_poverty = estimate_C18130_014,
    pwod_atorbelow_poverty = estimate_C18130_015,
    
    ### Affordability
    mortgage_burdened = estimate_B25091_008 + estimate_B25091_009 + estimate_B25091_010 + estimate_B25091_011,
    rent_burdened = estimate_B25070_007 + estimate_B25070_008 + estimate_B25070_009 + estimate_B25070_010,
    
    ### Full/Part Time Workers
    pop_ft = estimate_C18121_002,
    pwd_ft = estimate_C18121_003,
    pwod_ft = estimate_C18121_004,
    pop_not_ft = estimate_C18121_005,
    pwd_not_ft = estimate_C18121_006,
    pwod_not_ft = estimate_C18121_007,
    pop_didnotwork = estimate_C18121_008,
    pwd_didnotwork = estimate_C18121_009,
    pwod_didnotwork = estimate_C18121_010,
    
    ### Income
    pwd_grtoeq_16_med_individual_income = estimate_B18140_002,
    pwod_grtoeq_16_med_individual_income = estimate_B18140_005,
    
    ### Working from Home
    pct_pwd_grtoeq_16_wfm = estimate_S1811_C02_038,
    pct_pwod_grtoeq_16_wfm = estimate_S1811_C03_038
  ) %>%
  mutate(across(.cols = starts_with("pct"),.fns = ~ round(.x * 100, 2)))

### Variable dictionary
# pivot_longer the national_*_readable tables
# take all unique var names
national_varnames

# pivot_longer the national_* tables
# left_join national_Varnames to national_* tables by = c("GEOID", "NAME", "NAME_ABBRV")

