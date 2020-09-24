library(tidyverse);library(readxl)
library(tigris);library(sf);library(janitor)
library(tidycensus);library(viridis);library(tmap)
library(RPostgres);library(RPostgreSQL);library(DBI)

# Set tigris option to TRUE for Census geometries
# to load with ACS data call in first section of script
options(tigris_use_cache = TRUE)

# Must load census API key to call data
source("secret.R", local = TRUE)
census_api_key(api_key_census)

# Set working directory to current path
setwd(here::here())


# Import place names from Excel -------------------------------------------


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


# Identify states and counties for each Census designated place -----------


# Get Census designated places 
places <- map_df(.x = base_existing %>% pull(abbrev) %>% unique(),
                 .f = ~(tigris::places(state = .x, cb = TRUE, class = "sf") %>% 
                          rename("place_fips" = GEOID, "place_name" = NAME) %>% 
                          mutate("state" = .x,
                                 "metro_state" = paste0(place_name, ", ", state)) %>% 
                          select(place_fips, place_name, state, metro_state, everything()))) %>% 
  semi_join(., base_existing, by = "metro_state")

# Get counties associated with CDPs
counties <- map_df(.x = base_existing %>% pull(abbrev) %>% unique(),
                   .f = ~(tigris::counties(state = .x, cb = TRUE, class = "sf") %>% 
                            rename("county_fips" = GEOID, "county_name" = NAME) %>% 
                            mutate("state" = .x) %>% 
                            select(county_fips, county_name, state, everything())))
  
# Spatial join places and counties to get names of counties that intersect places
places_counties <- st_join(places, counties) 


# Generate vector of ACS variables to pull --------------------------------


# List of variables for reference
ref_vars <- load_variables(2018, "acs5", cache = TRUE)

# Get variables in table C21007
vars <- ref_vars %>% 
  filter(name == "B00001_001" | str_detect(concept, pattern = "DISABILITY STATUS"))


# Get spatial data for block groups ---------------------------------------


# Restrict block groups to those within CDP
block_groups_sf <- pmap_df(places_counties %>%
                             select("state_fips" = STATEFP.y,
                                    "county_fips" = COUNTYFP) %>% 
                             st_drop_geometry() %>% 
                             unique(),
                           .f = ~(get_acs(geography = "block group",
                                          year = 2018,
                                          variable = vars %>% 
                                            filter(name == "B00001_001") %>% pull(name),
                                          survey = "acs5",
                                          state = ..1, 
                                          county = ..2,
                                          geometry = TRUE))) %>%
  left_join(., vars, by = c("variable" = "name")) %>% 
  mutate(table_name = gsub( "_.*$", "", variable),
         label = gsub("!!", "; ", label)) %>% 
  filter(grepl("\\d$", table_name))

# Filter to block groups to those within CDPs
block_groups_sf <- block_groups_sf %>% 
  st_filter(., places) %>% 
  st_join(., places %>% select(place_fips, place_name, state, metro_state), 
          largest = TRUE, left = FALSE) 


# Write spatial data to database ------------------------------------------


# Connect to local database, can get properties from pgAdmin 
conn <- dbConnect(RPostgres::Postgres(), 
                  dbname = "ADA-PARC", 
                  host = "localhost", 
                  port = "5432",
                  user = "postgres", 
                  password = "Voorhees1")

# Write to database
dbWriteTable(conn = conn, 
             name = "geo_block_group", 
             value = block_groups_sf %>% 
               select("block_group_fips" = GEOID, "block_group_name" = NAME,
                      place_fips:metro_state,
                      concept, table_name, variable, label, estimate), 
             overwrite = TRUE)


# Get tabular data for block groups ---------------------------------------


# Get disability data for block groups in counties/states
block_groups_raw <- pmap_df(places_counties %>%
                              select("state_fips" = STATEFP.y,
                                     "county_fips" = COUNTYFP) %>% 
                              st_drop_geometry() %>% 
                              unique(),
                            .f = ~(get_acs(geography = "block group",
                                           variables = vars %>% pull(name),
                                           year = 2018,
                                           survey = "acs5",
                                           state = ..1, 
                                           county = ..2,
                                           geometry = FALSE))) %>% 
  semi_join(., block_groups_sf, by = "GEOID") %>% 
  left_join(., vars, by = c("variable" = "name")) %>% 
  mutate(table_name = gsub( "_.*$", "", variable),
         label = gsub("!!", "; ", label)) %>% 
  filter(grepl("\\d$", table_name))


# Write variable lookup to database ---------------------------------------


# Write variables and names to lookup
lookup_var <- block_groups_raw %>% 
  select(table_name, concept, variable, label) %>% 
  distinct()

# Write to database
dbWriteTable(conn = conn, 
             name = "asc_variable_lu", 
             value = lookup_var, 
             overwrite = TRUE)


# Write other tabular data to database ------------------------------------


# Write each ACS table to database separately
map(.x = block_groups_raw %>% 
      select(table_name) %>% 
      distinct() %>% 
      pull(),
    .f = ~(dbWriteTable(conn = conn, 
                        name = paste0("acs_", .x), 
                        value = block_groups_raw %>% 
                          filter(table_name == .x) %>% 
                          select(concept, 
                                 "block_group_fips" = GEOID, "block_group_name" = NAME,
                                 variable, label, estimate), 
                        overwrite = TRUE)))

# Disconnect
dbDisconnect(con)


# Map ---------------------------------------------------------------------


# #Plot persons w/ disabilities in Albuquerque, NM
# tm_shape(block_groups_sf %>% 
#            select(GEOID, metro_state) %>% 
#            filter(metro_state == "Albuquerque, NM") %>% 
#            left_join(., 
#                      block_groups_raw %>%
#                        filter(table_name == "B00001",
#                               variable == "B00001_001"), 
#                      by = "GEOID"), 
#          projection = 4326) +
#   tm_polygons("estimate",
#               style = "quantile",
#               palette = "Oranges") +
#   tm_legend(legend.position = c("left", "bottom")) +
#   tm_scale_bar() +
#   tm_compass() +
#   tm_layout(inner.margins = c(0.1, 0.1, 0.1, 0.1))
