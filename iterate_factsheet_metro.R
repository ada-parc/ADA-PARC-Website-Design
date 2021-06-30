

# Setup -------------------------------------------------------------------


# Libraries
library(rmarkdown); library(tidyverse)

# Pull location crosswalk dictionary from GitHub repo
factsheet_metro_params <- read_csv("https://raw.githubusercontent.com/sean-connelly/ADA-PARC-Website-Design/master/dictionaries/dict_location_crosswalk.txt") %>% 
  distinct("GEOID" = place_GEOID, metro_state)

# Full ACS cities list with population
cities_acs_places_ref <- read_csv("https://raw.githubusercontent.com/sean-connelly/ADA-PARC-Website-Design/master/dictionaries/dict_places.txt") %>% 
  filter(!is.na(POP))

# Join population data, separate metro_state for file name
factsheet_metro_params <- factsheet_metro_params %>% 
  left_join(cities_acs_places_ref %>% 
              distinct(GEOID, POP),
            by = "GEOID")

rm(cities_acs_places_ref)

