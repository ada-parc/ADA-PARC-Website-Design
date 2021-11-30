determineCompSets <- function(comp_var,
                              national_category_selector) {
  display_type <- dict_vars %>% 
    filter(var_readable == comp_var) %>% 
    pull(display_type)
  if(display_type != "comp") {return(NA)}
}

dict_vars <- read_csv(here("dictionaries", "dict_vars_edits.csv"))

# Test configs
# comp_var <- "pwd_18_64"
# national_category_selector <- "is_demographics"

# comp_var <- "pwod_19_64_insured_pct"
# national_category_selector <- "is_community_participation"

# comp_var <- ""
# national_category_selector <- ""


base_var <- str_extract(comp_var, "((?<=pwd_)|(?<=pwod_)).*$")
base_var

dict_vars %>%
  filter(
    var_readable == comp_var, 
    # !!sym(national_category_selector)
         ) %>%
  pull(display_type)

dict_vars <- dict_vars %>%
  mutate(var_base = ifelse(display_type == "comp", str_extract(var_readable, "((?<=pwd_)|(?<=pwod_)).*$"), NA))

write_csv(dict_vars, here("dictionaries", "dict_vars_edits.csv"))
