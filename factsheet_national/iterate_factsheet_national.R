

# Setup -------------------------------------------------------------------


# Libraries
library(rmarkdown); library(tidyverse);library(here)


# Factsheet parameters ----------------------------------------------------


# Pull baseline places in database from GitHub repo
# Organize parameters for looping operation
factsheet_national_params <- read_csv("https://raw.githubusercontent.com/sean-connelly/ADA-PARC-Website-Design/master/dictionaries/dict_vars.csv") %>%
  filter(!is.na(national_dropdown_label)) %>% 
  select(starts_with("is_"), 
         "national_variable_selector" = var_readable,
         national_dropdown_label) %>% 
  pivot_longer(cols = -c(national_variable_selector,
                         national_dropdown_label),
               names_to = "national_category_selector") %>%
  filter(value == TRUE) %>% 
  select(national_category_selector,
         national_variable_selector,
         national_dropdown_label) %>% 
  group_by(national_dropdown_label) %>% 
  slice(1) %>% 
  ungroup() %>%
  mutate("output_file" = stringr::str_c(here(), "/factsheet_national/output_files/", 
                                        national_dropdown_label, ".html"),
         "params" = pmap(list(national_category_selector,
                              national_variable_selector), 
                        ~list(national_category_selector = ..1,
                              national_variable_selector = ..2))) %>% 
  filter(national_variable_selector %in% c("pop_total",
                                           "pwd_commute_car_alone_pct",
                                           "pwd_commute_public_pct"))


# Create factsheets -------------------------------------------------------


# Walk through dataframe, create documents
factsheet_national_params %>%
  select(output_file, params) %>% 
  pwalk(rmarkdown::render, 
        input = stringr::str_c(here(), "/factsheet_national/factsheet_national.Rmd"))
