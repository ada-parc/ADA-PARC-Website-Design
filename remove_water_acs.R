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

# Load in geometry data from database table here
# ***HERE***


# Erase water from geometries ---------------------------------------------


# Get water data from TIGRIS
water_sf <- pmap_df(places_counties %>%
                      select("state_fips" = STATEFP.y,
                             "county_fips" = COUNTYFP) %>%
                      st_drop_geometry() %>%
                      unique(),
                    .f = ~(tigris::area_water(state = ..1,
                                              county = ..2,
                                              class = "sf")))

st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

block_groups_sf <- st_erase(block_groups_sf, water_sf)