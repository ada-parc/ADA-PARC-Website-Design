

# Setup -------------------------------------------------------------------


# Libraries
library(tidyverse)
library(tigris);library(sf)
library(tidycensus)
library(geojsonsf);library(mongolite);library(config)

# Set working dictionary, options, increase memory limit
memory.limit(size = 10000)

host_name <- "host_dev" # or "host_dev"

# Load API keys and database connection information
source(here::here("scripts", "functions.R"), local = TRUE)

# Parameters for ACS call -------------------------------------------------


### ----- Cities -----


# Load baseline dictionary of places 
cities_acs_places_selected <- read_csv(
  here::here("dictionaries",
       "baseline_places_geoid_upload.csv")
  ) %>%
  rename("GEOID" = 1) %>%
  mutate("GEOID" = as.character(GEOID) %>% 
           stringr::str_pad(7, side = "left", pad = "0"))


### ----- ACS variables -----


# Tract reference variables
tracts_acs_vars_ref <- 
  load_variables(year = 2019,
                 dataset = "acs5/subject", 
                 cache = TRUE) %>% 
    filter(!is.na(concept)) %>% 
    mutate("table_name" = gsub( "_.*$", "", name),
           "label" = gsub("!!", "; ", label),
           "shiny_input_label" = paste0(table_name, ": ",
                                        str_to_title(concept)))

# Load baseline dictionary of variables for tracts 
file_upload <- read_csv(
  here::here("dictionaries",
       "baseline_tract_vars_upload.csv")
) %>% 
  rename("var_name" = 1)

# Filter variables based on baseline variable selection
tracts_acs_vars_selected <- tracts_acs_vars_ref %>% 
      filter(name %in% file_upload$var_name) %>%
      filter(!str_detect(label,
                         pattern = "DISABILITY TYPE BY DETAILED AGE")) # Removes detailed age data
   
  
### ----- State/county pairs for tracts -----


# Pull city/county/tract dictionary from GitHub repo 
# Set minimum overlap threshold, default units [m^2]
tracts_places_counties <- read_csv("https://raw.githubusercontent.com/ada-parc/ADA-PARC-Website-Design/master/dictionaries/dict_location_crosswalk.txt") %>%
    filter(county_place_overlap_pct > 0,
           place_GEOID %in% c(cities_acs_places_selected$GEOID))
    


# Download data -----------------------------------------------------------


### ----- ACS subject tables -----


# Download subject tables for tracts
acs_raw <- pmap_df(
  .l = tracts_places_counties %>%
    mutate("STATEFP" = str_sub(county_GEOID, 1, 2),
           "COUNTYFP" = str_sub(county_GEOID, 3, 5)) %>% 
    select(STATEFP, COUNTYFP) %>% 
    distinct(),
  .f = ~(get_acs(geography = "tract",
                 year = 2019,
                 variables = tracts_acs_vars_selected %>% 
                   pull(name),
                 dataset = "acs5/subject",
                 state = ..1, 
                 county = ..2,
                 geometry = FALSE,
                 output = "tidy")))


### ----- Tract geographies -----


# Tracts simple features df
# Use dictionary and filter to selected tracts
tracts_geography <- st_read(here::here(
  "dictionaries",
  "/geo_tract/geo_tract.shp")) %>%
select("tract_GEOID" = t_GEOID,
       "tract_NAME" = tr_NAME) %>%
  filter(tract_GEOID %in% tracts_places_counties$tract_GEOID)


### ----- ACS output (tabular and spatial) -----


# Restrict tabular data using filtered tracts
# Return list with [[1]] = data, [[2]] = geography
acs_output <- list("data" = acs_raw %>% 
                     filter(GEOID %in% tracts_geography$tract_GEOID),
                   "geography" = tracts_geography)


# Export data -------------------------------------------------------------


### ----- Write to ZIP file -----


# ***Note: final ZIP command here failed manually given size of data CSV, had to be zipped manually

# # ZIP file path
# zip_to_folder <- "Path to Downloads/other folder"
# 
# # Files
# files = c("tracts_export_data.csv",
#           "tracts_export_lookup.csv",
#           "tracts_export_geography.shp")
# 
# # Excel CSVs
# # Data
# write_csv(acs_output[["data"]],
#           paste0(zip_to_folder,
#                  "tracts_export_data.csv"))
# # Lookup
# write_csv(tracts_acs_vars_selected %>% 
#             select(-shiny_input_label),
#           paste0(zip_to_folder, 
#                  "tracts_export_lookup.csv"))
# 
# # Shapefile
# sf::st_write(acs_output[["geography"]],
#              paste0(zip_to_folder,
#                     "tracts_export_geography.shp"),
#              delete_dsn = TRUE)
# 
# # ZIP
# zip::zip(paste0(zip_to_folder, "tracts_export_files.zip"),
#          paste0(zip_to_folder, files))
  

### ----- Write to MongoDB -----


# # Run write table function
# fun_write_by_geo_table(df = acs_output[["data"]], 
#                        host_name = host_name,
#                        geo = "tract")
# 
# # Upload geographies
# # Connect to MongoDB database/collection
# mongo_conn <- fun_mongo_connect(host_name = host_name,
#                                 collection_name = "geo_tract",
#                                 database_name = "ADA-PARC")
# 
# # Write to database and add spatial index
# temp_geo <- geojsonsf::sf_geojson(acs_output[["geography"]], 
#                                   atomise = TRUE) 
# mongo_conn$drop()
# mongo_conn$insert(temp_geo)
# mongo_conn$index((add = '{"geometry" : "2dsphere"}'))
# 
# # Disconnect
# rm(temp_geo)
# rm(mongo_conn)




    