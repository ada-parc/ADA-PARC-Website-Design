
# Load dictionary
dict_vars <- read_csv(here("dictionaries", "dict_vars.csv"))

dict_vars <- dict_vars %>% 
  mutate("national_summary_text" = ifelse(is.na(national_summary_text),
                                          "",
                                          national_summary_text))

# Write to database
mongo_conn <- fun_mongo_connect(host_name = "host_dev",
                                collection_name = "dict_vars",
                                database_name = "ADA-PARC")

mongo_conn$drop()
mongo_conn$insert(dict_vars)