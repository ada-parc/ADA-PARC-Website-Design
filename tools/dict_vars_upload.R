
# Libraries
library(tidyverse);library(here)
library(mongolite)

# Load API keys and database connection information
source(here("scripts", "functions.R"), local = TRUE)

# Load dictionary
dict_vars <- read_csv(here("dictionaries", "dict_vars.csv"))

dict_vars <- dict_vars %>% 
  mutate("national_summary_text" = ifelse(is.na(national_summary_text),
                                          "",
                                          national_summary_text))

# Check for summary text characters
# spec_char_check <- dict_vars %>% 
#   filter(national_summary_text != "") %>% 
#   mutate("row_number" = row_number()) %>% 
#   select( # row_number,var_readable, 
#          national_summary_text)
# 
# spec_char_check

# Write to database
mongo_conn <- fun_mongo_connect(host_name = "host_dev",
                                collection_name = "dict_vars",
                                database_name = "ADA-PARC")

mongo_conn$drop()
mongo_conn$insert(dict_vars)
