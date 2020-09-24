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
  filter(str_detect(name, pattern = "^C21007_*"))


# Get spatial data for block groups ---------------------------------------


# Restrict block groups to those within CDP
block_groups_sf <- pmap_df(places_counties %>%
                             select("state_fips" = STATEFP.y,
                                    "county_fips" = COUNTYFP) %>% 
                             st_drop_geometry() %>% 
                             unique(),
                           .f = ~(get_acs(geography = "block group",
                                          year = 2018,
                                          variables = "B00001_001",
                                          survey = "acs5",
                                          state = ..1, 
                                          county = ..2,
                                          geometry = TRUE,
                                          output = "wide"))) %>% 
  rename("total_population" = B00001_001E)

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
                      place_fips:metro_state, total_population), 
             overwrite = TRUE)


# Get tabular data for block groups ---------------------------------------

# Example for getting Subject Tables at Tract level
# tracts_raw <- pmap_df(expand_grid(places_counties %>%
#                                     select("state_fips" = STATEFP.y,
#                                            "county_fips" = COUNTYFP) %>% 
#                                     st_drop_geometry() %>% 
#                                     unique(), 
#                                   table = c("S1810", "S1811")),
#                       .f = ~(get_acs(geography = "tract",
#                                      year = 2018,
#                                      table = ..3,
#                                      survey = "acs5",
#                                      state = ..1, 
#                                      county = ..2,
#                                      geometry = FALSE,
#                                      wide = TRUE)))

# Get disability data for block groups in counties/states
block_groups_raw <- pmap_df(places_counties %>%
                              select("state_fips" = STATEFP.y,
                                     "county_fips" = COUNTYFP) %>% 
                              st_drop_geometry() %>% 
                              unique(),
                            .f = ~(get_acs(geography = "block group",
                                           year = 2018,
                                           variables = vars %>% pull(name),
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
             name = "acs_variable_lu", 
             value = lookup_var, 
             overwrite = TRUE)


# Calculate PWD estimates -------------------------------------------------


#Total estimates
block_groups_total <- block_groups_raw %>%
  filter(variable == "C21007_001") %>%
  group_by(GEOID, NAME) %>% 
  summarize("total_est" = sum(estimate), 
            total_moe = moe_sum(moe = moe, 
                                estimate = estimate))

#Disability estimates
block_groups_pwd <- block_groups_raw %>%
  filter(str_detect(label, pattern = "With a disability$")) %>%
  group_by(GEOID, NAME) %>% 
  summarize("pwd_est" = sum(estimate), 
            "pwd_moe" = moe_sum(moe = moe, 
                                estimate = estimate))

#Join totals per block group to disability estimates
block_groups_pwd <- left_join(block_groups_pwd, block_groups_total, 
                        by = c("GEOID", "NAME"))

#Calculate % disability
block_groups_pwd <- block_groups_pwd %>%
  ungroup() %>% 
  mutate("pwd_perc_est" = pwd_est/total_est,
         "pwd_perc_moe" = moe_ratio(num = pwd_est, 
                                    denom = total_est,
                                    moe_num = pwd_moe, 
                                    moe_denom = total_moe))


# Write other tabular data to database ------------------------------------


# Example of writing several variables out to separate tables by code
# map(.x = block_groups_raw %>% 
#       select(table_name) %>% 
#       distinct() %>% 
#       pull(),
#     .f = ~(dbWriteTable(conn = conn, 
#                         name = paste0("acs_", .x), 
#                         value = block_groups_raw %>% 
#                           filter(table_name == .x) %>% 
#                           select(concept, 
#                                  "block_group_fips" = GEOID, "block_group_name" = NAME,
#                                  variable, label, estimate) %>% 
#                           pivot_wider(names_from = ), 
#                         overwrite = TRUE)))

# Write each ACS table to database separately
dbWriteTable(conn = conn, 
             name = "acs_C21007", 
             value = block_groups_pwd, 
             overwrite = TRUE)

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
