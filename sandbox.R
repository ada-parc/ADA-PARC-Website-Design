determineCompVar <- function(comp_var,
                              national_category_selector) {
  if(!exists("dict_vars")) {
    stop("dict_vars not loaded")
    }
  
  display_type <- dict_vars %>% 
    filter(var_readable == comp_var, !!sym(national_category_selector)) %>% 
    pull(display_type)
  
  if(display_type != "comp") 
  {
    return(NA)
  } else {
    base_var <- dict_vars %>% 
      filter(var_readable == comp_var, !!sym(national_category_selector)) %>% 
      pull(var_base)
    
    dict_vars %>%
      filter(var_base == base_var, var_readable != comp_var) %>%
      pull(var_readable)
  }

}

dict_vars <- read_csv(here("dictionaries", "dict_vars_edits.csv"))

# Test configs
# comp_var <- "pwd_18_64"
# national_category_selector <- "is_demographics"

comp_var <- "pwod_19_64_insured_pct"
national_category_selector <- "is_community_participation"

# comp_var <- ""
# national_category_selector <- ""


base_var <- str_extract(comp_var, "((?<=pwd_)|(?<=pwod_)).*$")
# base_var

# Logic to find the display type given a var and category
dict_vars %>%
  filter(
    var_readable == comp_var, 
    !!sym(national_category_selector)
         ) %>%
  pull(display_type)

# Logic to return the other variable
dict_vars %>%
  filter(var_base == base_var, var_readable != comp_var) %>%
  pull(var_readable)

# Create a var_base value
# dict_vars <- dict_vars %>%
#   mutate(var_base = ifelse(display_type == "comp", str_extract(var_readable, "((?<=pwd_)|(?<=pwod_)).*$"), NA))

# write_csv(dict_vars, here("dictionaries", "dict_vars_edits.csv"))
