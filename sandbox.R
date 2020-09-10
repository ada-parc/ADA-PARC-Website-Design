library(tidyverse); library(readxl); library(openxlsx); library(summarytools); library(janitor); 
library(sf); library(urbnmapr); library(leaflet); library(leaflet.extras);
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
demo_metro_data <- demographics %>% 
  filter(is.na(city) == FALSE) %>% 
  select("state_name" = state, "state_abbv" = abbrev, city,
         percent_of_total_population_with_a_disability, pwd, total_population, everything()) %>% 
  mutate(percent_of_total_population_with_a_disability =
           round(percent_of_total_population_with_a_disability, 1))


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
  
### Since the var_select can be infinitely large, I'm thinking that creating a ggplot() + geom_col() string using something like this: 
  for(i in seq(1, length(var_select))) {
    paste
  }
  