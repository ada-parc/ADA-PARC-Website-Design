dict_vars <- read_csv(here("dictionaries", "dict_vars.csv"))

mongo_conn <- fun_mongo_connect(host_name = "host_dev",
                                collection_name = "dict_vars",
                                database_name = "ADA-PARC")

mongo_conn$drop()
mongo_conn$insert(dict_vars)
