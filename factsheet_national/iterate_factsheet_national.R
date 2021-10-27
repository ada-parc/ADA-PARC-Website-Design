

# Setup -------------------------------------------------------------------


# Libraries
library(rmarkdown); library(tidyverse)
library(pagedown);library(mongolite); library(here)

# Set this to host_prod or host_dev
host_name = "host_dev"

# Load API keys and database connection information
source(here("functions.R"), local = TRUE)

# Load variable dictionary for relating tables, variable codes,
# and readable values
dict_vars <- fun_pull_mongo_data(tables = "dict_vars", 
                                 host_name = host_name)


# Factsheet parameters ----------------------------------------------------


# Pull baseline places in database from GitHub repo
# Organize parameters for looping operation
factsheet_national_params <- dict_vars %>%
  mutate("row_number_temp" = row_number()) %>% 
  filter(!is.na(national_dropdown_label)) %>% 
  select(row_number_temp,
         starts_with("is_"), 
         "national_variable_selector" = var_readable,
         national_dropdown_label) %>% 
  pivot_longer(cols = -c(row_number_temp,
                         national_variable_selector,
                         national_dropdown_label),
               names_to = "national_category_selector") %>%
  filter(value == TRUE) %>% 
  select(row_number_temp,
         national_category_selector,
         national_variable_selector,
         national_dropdown_label) %>% 
  group_by(national_dropdown_label) %>% 
  slice(1) %>% 
  ungroup() %>%
  arrange(row_number_temp) %>% 
  mutate("row_number" = stringr::str_pad(as.character(row_number()), 3, 
                                         side = "left", pad = "0")) %>% 
  mutate("output_file" = stringr::str_c(here(), 
                                        "/factsheet_national/output_files/",
                                        row_number, "_",
                                        national_dropdown_label, ".html"),
         "params" = pmap(list(national_category_selector,
                              national_variable_selector), 
                         ~list(national_category_selector = ..1,
                               national_variable_selector = ..2))) %>% 
  select(row_number, national_category_selector:params) 


# Restrict to subset for testing ------------------------------------------


# Create 3 factsheets to make sure of formatting 
factsheet_national_params <- factsheet_national_params %>% 
  filter(national_variable_selector %in% c("pop_total",
                                           "pwd_commute_car_alone_pct",
                                           "pwd_commute_public_pct"))


# Create factsheets -------------------------------------------------------


# Delete existing HTML and PDF files
file.remove(list.files(stringr::str_c(here(), 
                                      "/factsheet_national/output_files/"),
                       pattern = ".(html|pdf)$", 
                       full.names = TRUE)
            )

# Walk through dataframe, create HTML documents
factsheet_national_params %>%
  select(output_file, params) %>% 
  pwalk(rmarkdown::render, 
        input = stringr::str_c(here(), 
                               "/factsheet_national/factsheet_national.Rmd"))


# Get a vector of all the filepaths for the newly-generated HTML files
html_files <- list.files(stringr::str_c(here(), 
                                        "/factsheet_national/output_files/"),
                         pattern = ".html$", 
                         full.names = TRUE)

# Print HTMLs to PDFs
map(.x = html_files, 
    .f = ~chrome_print(input = .x))

# Delete HTML files
file.remove(list.files(stringr::str_c(here(), 
                                      "/factsheet_national/output_files/"),
                       pattern = ".html$", 
                       full.names = TRUE)
            )

