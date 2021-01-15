library(tidyverse);library(readxl)
library(tigris);library(sf);library(janitor)
library(tidycensus);library(viridis);library(tmap)
library(RPostgres);library(RPostgreSQL);library(DBI)
library(geojsonsf);library(mongolite)

# Set tigris option to TRUE for Census geometries
# to load with ACS data call in first section of script
options(tigris_use_cache = TRUE)

# Load API keys and database connection information
source("secret.R", local = TRUE)
census_api_key(api_key_census)

# Set working directory to current path
setwd(here::here())

###
# Import place names from Excel -------------------------------------------
###

# Import list of existing places used in analysis
db_path <- "February 2020 update 2018 ACS/2.20.20DatabaseACS2018.final.xlsx"

base_existing <- read_excel(path = db_path,
                            sheet = "Demographics", 
                            .name_repair = make_clean_names,
                            skip = 2) %>% 
  remove_empty(c("rows", "cols")) %>% 
  filter(!is.na(city), city != "United States")  %>% 
  rename("abbrev" = state_abbreviation) %>% 
  mutate(across(-c(state:city), as.numeric),
         "metro_state" = paste0(city, ", ", abbrev)) %>% 
  mutate(metro_state = case_when(metro_state == "Arlington Heights Village, IL" ~
                                   "Arlington Heights, IL",
                                 metro_state == "Hazel Crest Village, IL" ~
                                   "Hazel Crest, IL",
                                 metro_state == "Indianapolis, IN" ~
                                   "Indianapolis city (balance), IN",
                                 metro_state == "Naperville City, IL" ~
                                   "Naperville, IL",
                                 metro_state == "Nashville-Davidson, TN" ~
                                   "Nashville-Davidson metropolitan government (balance), TN",
                                 TRUE ~ metro_state)) %>% 
  select(state, abbrev, city, metro_state, everything())

###
# Get most populated places -----------------------------------------------
###

# Restrict to 200 most populated cities
most_populated <- get_estimates(geography = "place",
                                year = 2018,
                                product = "population",
                                geometry = FALSE,
                                output = "wide") %>% 
  arrange(desc(POP)) %>% 
  head(200)
  
###
# Identify states and counties for each Census designated place -----------
###

# All states and counties
fips_codes <- force(fips_codes) %>% 
  mutate("state_code_num" = as.numeric(state_code)) %>% 
  filter(state_code_num %in% c(1:56)) %>% 
  distinct(state, state_code, county_code)

# Get Census designated places in established lists
places <- pmap_df(.l = fips_codes %>% select(state_code, state) %>% distinct(),
                  .f = ~(tigris::places(state = ..1, cb = TRUE, class = "sf") %>% 
                           rename("place_fips" = GEOID, "place_name" = NAME) %>% 
                           mutate("state" = ..2,
                                  "metro_state" = paste0(place_name, ", ", state)) %>% 
                           select(place_fips, place_name, state, metro_state, everything()))) %>% 
  filter(metro_state %in% base_existing$metro_state | place_fips %in% most_populated$GEOID)

# Get counties associated with CDPs
counties <- pmap_df(.l = fips_codes %>% select(state_code, state) %>% distinct(),
                    .f = ~(tigris::counties(state = ..1, cb = TRUE, class = "sf") %>% 
                             rename("county_fips" = GEOID, "county_name" = NAME) %>% 
                             mutate("state" = ..2) %>% 
                             select(county_fips, county_name, state, everything())))
  
# Spatial join places and counties to get names of counties that intersect places
places_counties <- st_join(places, counties) 

###
# Get place ACS data ------------------------------------------------------
###

# List of variables for reference
ref_vars_subject <- load_variables(2018, "acs5/subject", cache = TRUE)

# Get variables in subject tables
acs_place_subject_vars <- ref_vars_subject %>% 
  filter(str_detect(name, pattern = "(^S181(0|1)_*)|(^S26(01A|02)_*)")) %>% 
  filter(!str_detect(label, pattern = "DISABILITY TYPE BY DETAILED AGE")) %>% # Removes detailed age data
  mutate(table_name = gsub( "_.*$", "", name),
         label = gsub("!!", "; ", label))

# Grab place-level data
acs_place_subject_raw <- pmap_df(.l = fips_codes %>% select(state_code) %>% distinct(),
                                 .f = ~(get_acs(geography = "place",
                                                year = 2018,
                                                variables = acs_place_subject_vars %>% pull(name),
                                                survey = "acs5",
                                                state = ..1, 
                                                geometry = FALSE,
                                                wide = TRUE) %>% 
                                          semi_join(., places, by = c("GEOID" = "place_fips"))))

### 
### Get state ACS data -----------------------------------------------------
###

# List of acs5 variables for reference
# Pull B/C vars to bind with S vars used with Places
ref_vars <- load_variables(2018, "acs5", cache = TRUE)

# Get variable tables to query from acs5/subject and acs5
# Does not remove detailed age data from subject tables
acs_subject_vars <- ref_vars_subject %>% 
  filter(str_detect(name, pattern = "(^S181(0|1)_*)|(^S26(01A|02)_*)")) %>% 
  mutate(table_name = gsub( "_.*$", "", name),
         label = gsub("!!", "; ", label))

acs_vars <- ref_vars %>%
  filter(str_detect(name, pattern = "(^B181(40|35)_*)|(^C181(20|30|21)_*)")) %>% 
  mutate(table_name = gsub( "_.*$", "", name),
         label = gsub("!!", "; ", label))
  
acs_vars_full <- bind_rows(acs_subject_vars, acs_vars)

# Grab state-level data
acs_state_raw <- get_acs(geography = "state",
                                 year = 2018,
                                 variables = acs_vars_full %>% pull(name),
                                 survey = "acs5",
                                 geometry = FALSE,
                                 wide = TRUE) %>%
  # Standardize variable names for db
  mutate(variable = gsub("PR", "", variable)) %>%
  left_join(fips_codes %>% select(state, state_code), by = c("GEOID" = "state_code")) %>%
  relocate(state, .after = NAME) %>%
  distinct()

###
### Write variable lookup to database ---------------------------------------
###

# Write variables and names to lookup
# acs_vars_full contains every variable used between state, place, and tract
lookup_var <- acs_vars_full %>% 
  select(concept, table_name, "variable" = name, label) %>% 
  # PR has its own table codes, changing those codes to the standards for the table
  mutate(
    table_name = gsub("PR", "", table_name),
    variable = gsub("PR", "", variable)
  ) %>%
  distinct()

# Connect to MongoDB database/collection
mongo_conn <- mongo_connect(collection_name = "acs_variable_lu",
                            database_name = "ADA-PARC")

# Write to database and add spatial index
mongo_conn$drop()
mongo_conn$insert(lookup_var)

# Disconnect
rm(mongo_conn)

###
### Get spatial data for tracts ---------------------------------------------
###

# Restrict tracts to those within CDP
tracts_sf <- pmap_df(places_counties %>%
                       select("state_fips" = STATEFP.y,
                              "county_fips" = COUNTYFP) %>% 
                       st_drop_geometry() %>% 
                       unique(),
                     .f = ~(get_acs(geography = "tract",
                                    year = 2018,
                                    variables = "B00001_001",
                                    survey = "acs5",
                                    state = ..1, 
                                    county = ..2,
                                    geometry = TRUE,
                                    output = "wide"))) %>% 
  rename("total_population" = B00001_001E)

# Filter to block groups to those within CDPs
tracts_sf <- tracts_sf %>% 
  st_filter(., places) %>% 
  st_join(., places %>% select(place_fips, place_name, state, metro_state), 
          largest = TRUE, left = FALSE) 

# Remove temporary tables
rm(places, counties)

###
### Write spatial data to database ------------------------------------------
###

# Connect to MongoDB database/collection
mongo_conn <- mongo_connect(collection_name = "geo_tract",
                            database_name = "ADA-PARC")

# Write to database and add spatial index
temp_geo <- geojsonsf::sf_geojson(tracts_sf, atomise = TRUE) 
mongo_conn$drop()
mongo_conn$insert(temp_geo)
mongo_conn$index((add = '{"geometry" : "2dsphere"}'))

# Disconnect
rm(temp_geo)
rm(mongo_conn)

###
### Get tabular data for tracts ---------------------------------------------
###

# Get variables in subject tables (subset of place variables)
acs_tract_subject_vars <- acs_place_subject_vars %>% 
  filter(str_detect(name, pattern = "^S181(0|1)_*"))

# Get subject tables at tract level
acs_tract_subject_raw <- pmap_df(places_counties %>%
                                   select("state_fips" = STATEFP.y,
                                          "county_fips" = COUNTYFP) %>%
                                   st_drop_geometry() %>%
                                   unique(),
                                 .f = ~(get_acs(geography = "tract",
                                                year = 2018,
                                                variables = acs_tract_subject_vars %>% pull(name),
                                                survey = "acs5",
                                                state = ..1,
                                                county = ..2,
                                                geometry = FALSE,
                                                wide = TRUE) %>% 
                                          semi_join(., tracts_sf, by = "GEOID")))

###
### Function to write data efficiently to MongoDB ---------------------------
###

# Organize data into list of dfs and export
fun_write_by_geo_table <- function(df, geo_name) {
  
  # Group by table name
  df <- df %>% 
    left_join(lookup_var %>% select(variable, table_name), by = "variable") %>% 
    group_by(table_name) %>% 
    drop_na(estimate) %>% 
    distinct()
  
  # Get table names to rename list elements 
  df_group_names <- group_keys(df) %>% 
    mutate("table_name" = paste0("acs_", geo_name, "_", table_name))
  
  # Reconfigure into list of dfs for database purposes
  df_list <- df %>% 
    group_map( ~{ .x %>% 
    pivot_wider(names_from = variable, values_from = c(estimate, moe)) },
    .keep = FALSE)
  
  # Set names for list
  df_list <- df_list %>% 
    setNames(df_group_names$table_name)

  # Write to MongoDB function
  map2(.x = df_list,
       .y = names(df_list),
       .f = ~{
         # Connect to MongoDB database/collection
         mongo_conn <- mongo_connect(collection_name = .y,
                                     database_name = "ADA-PARC")
         
         # Write to database
         mongo_conn$drop()
         mongo_conn$insert(.x)
         
         # Message
         msg <- paste0("Writing ", as.character(nrow(.x)),
                       " rows to ", .y)
         print(msg)
         
         # Disconnect
         rm(mongo_conn)
       }
  )
  
}

###
### Export to MongoDB -------------------------------------------------------
###

# States
fun_write_by_geo_table(df = acs_state_raw, geo = "state")

# Places
fun_write_by_geo_table(df = acs_place_subject_raw, geo = "place")

# Tracts
fun_write_by_geo_table(df = acs_tract_subject_raw, geo = "tract")

# Remove temporary tables
rm(acs_place_subject_vars, acs_tract_subject_vars, tracts_sf)