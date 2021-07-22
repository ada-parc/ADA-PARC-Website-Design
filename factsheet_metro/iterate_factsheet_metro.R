

# Setup -------------------------------------------------------------------


# Libraries
library(rmarkdown); library(tidyverse);library(here)


# Factsheet parameters ----------------------------------------------------


# Pull baseline places in database from GitHub repo
# Organize parameters for looping operation
factsheet_metro_params <- read_csv("https://raw.githubusercontent.com/sean-connelly/ADA-PARC-Website-Design/master/dictionaries/baseline_places_geoid_upload.csv") %>%
  separate(metro_state, into = c("metro", "state"),
           sep = ", ", remove = FALSE) %>% 
  select("GEOID" = census_GEOID, metro_state, 
         metro, state, population) %>% 
  mutate("GEOID" = as.character(GEOID) %>% 
           stringr::str_pad(7, side = "left", pad = "0"),
         "output_file" = stringr::str_c(here(), "/factsheet_metro/output_files/", 
                                        metro, "_", state, ".html"),
         "params" = map(GEOID, ~list(GEOID = .))) %>% 
  filter(GEOID %in% c("1714000", "3651000", "2507000")) %>% 
  arrange(state, metro)


# Create factsheets -------------------------------------------------------


# Walk through dataframe, create documents
factsheet_metro_params %>%
  select(output_file, params) %>% 
  pwalk(rmarkdown::render, 
        input = stringr::str_c(here(), "/factsheet_metro/factsheet_metro.Rmd"))
