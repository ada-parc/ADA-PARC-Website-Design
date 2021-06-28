

# Setup -------------------------------------------------------------------


# Libraries
library(tidyverse);library(readxl)
library(tidycensus);library(tigris);library(sf)
library(geojsonsf);library(mongolite)

# Set working dictionary, options
setwd(here::here())
options(stringsAsFactors = FALSE, scipen = 999, 
        dplyr.summarise.inform = FALSE, tigris_use_cache = TRUE)

# Load API keys
api_key_census <- config::get("api_key_census")


# FIPS codes --------------------------------------------------------------


# All states and counties
fips_codes_tidy <- force(fips_codes)


# Places/Cities lookup ----------------------------------------------------


# Get spatial footprint of cities/places
# Remove US Outlying Islands/US Virgin Islands (c(1:56, 72))
places_sf <- pmap_df(.l = fips_codes_tidy %>% 
                       filter(as.numeric(state_code) %in% c(1:56, 72)) %>% 
                       select(state_code) %>% 
                       distinct(),
                     .f = ~(tigris::places(state = ..1, 
                                           cb = TRUE, 
                                           class = "sf") %>% 
                              rename("place_GEOID" = GEOID, 
                                     "place_NAME" = NAME))) %>%
  select(STATEFP, PLACEFP, place_GEOID, place_NAME) %>% 
  mutate("place_area" = st_area(.),
         "place_area_num" = as.numeric(place_area)) %>% 
  relocate(geometry, .after = last_col())

# Get population estimates for lookup
places_pop_est <- get_estimates(geography = "place",
                                product = "population",
                                geometry = FALSE,
                                output = "wide")

# Clean for lookup
dict_places <- places_sf %>% 
  st_drop_geometry() %>%
  select(STATEFP, "GEOID" = place_GEOID) %>% 
  # Population to order list
  left_join(places_pop_est,
            by = "GEOID") %>% 
  # Metro/State names
  left_join(fips_codes_tidy %>% 
              select(state_code, state) %>% 
              distinct(),
            by = c("STATEFP" = "state_code")) %>% 
  # Basic scrubbing
  mutate("metro_state" = paste0(str_remove(NAME, ",.*$"),
                                ", ", state) %>% 
           str_replace_all(.,
                           pattern = " (city|village|municipality|town|city and borough|borough|(city|((unified|consolidated|metro|metropolitan) government)) \\(balance\\)|\\(balance\\)), ",
                           replacement = ", ")) %>% 
  # Individual cases
  mutate("metro_state" = case_when(GEOID == "3651000" ~
                                     "New York City, NY",
                                   GEOID == "4752006" ~
                                     "Nashville, TN",
                                   GEOID == "1571550" ~
                                     "Honolulu, HI",
                                   GEOID == "2146027" ~
                                     "Lexington, KY",
                                   GEOID == "2148006" ~
                                     "Louisville, KY",
                                   TRUE ~
                                     metro_state)) %>% 
  select(GEOID, NAME, metro_state,
         POP, DENSITY)



# Places, counties, tracts crosswalk --------------------------------------


## ----- Get spatial footprint of counties -----
# Remove US Outlying Islands/US Virgin Islands (c(1:56, 72))
counties_sf <- pmap_df(.l = places_sf %>%
                      st_drop_geometry() %>% 
                      select(STATEFP) %>% 
                      distinct(),
                    .f = ~(tigris::counties(state = ..1, 
                                            cb = TRUE, 
                                            class = "sf") %>% 
                             rename("county_GEOID" = GEOID, 
                                    "county_NAME" = NAME))) %>% 
  select(COUNTYFP, county_GEOID, county_NAME)  


## ----- Unique city/state/county codes -----
# Filter counties by joining to cities
# Set minimum overlap threshold, default units [m^2]
places_counties <- st_intersection(places_sf, 
                                   counties_sf) %>% 
  mutate("overlap_area" = st_area(.),
         "overlap_area_num" = as.numeric(overlap_area),
         "overlap_pct" = overlap_area_num / place_area_num) %>%
  filter(overlap_pct > 0) %>%
  st_drop_geometry()


## ----- Tracts -----


# Tracts sf dataframe
tracts_sf <- pmap_df(.l = places_counties %>% 
                       select(STATEFP, COUNTYFP) %>% 
                       distinct(),
                     .f = ~(tigris::tracts(state = ..1,  
                                           county = ..2,
                                           cb = TRUE, 
                                           class = "sf"))) %>% 
  select("tract_GEOID" = GEOID, 
         "tract_NAME" = NAME, 
         STATEFP, COUNTYFP) %>% 
  mutate("tract_area" = st_area(.),
         "tract_area_num" = as.numeric(tract_area)) %>% 
  relocate(geometry, .after = last_col())


# Filter counties by joining to cities
# Set minimum overlap threshold, default units [m^2]
places_tracts <- st_intersection(places_sf, 
                                 tracts_sf) %>% 
  mutate("overlap_area" = st_area(.),
         "overlap_area_num" = as.numeric(overlap_area),
         "overlap_pct" = overlap_area_num / place_area_num) %>%
  filter(overlap_pct > 0) %>%
  st_drop_geometry()


## ----- Join place/county/tract ----- 


# Crosswalk, left join above
dict_location_crosswalk <- places_sf %>%
  st_drop_geometry() %>%
  select(place_GEOID) %>%
  # Clean place name
  left_join(dict_places %>% 
              rename("place_GEOID" = GEOID),
            by = "place_GEOID") %>% 
  # Tracts
  left_join(places_tracts %>% 
              select(place_GEOID, STATEFP, COUNTYFP, 
                     tract_GEOID, tract_NAME,
                     "tract_place_overlap_pct" = overlap_pct),
            by = "place_GEOID") %>% 
  # Counties joined to correct tracts
  left_join(places_counties %>% 
              select(place_GEOID, STATEFP, COUNTYFP,
                     county_GEOID, county_NAME,
                     "county_place_overlap_pct" = overlap_pct),
            by = c("place_GEOID", "STATEFP", "COUNTYFP")) %>% 
  select(place_GEOID, metro_state, 
         starts_with("county_"), starts_with("tract_"))
  
# Spatial join check (don't want counties that only border included)
# mapview::mapshot(
# mapview::mapview(places,
#                  col.regions = "orange") +
#   mapview::mapview(counties %>%
#                      filter(county_GEOID %in%
#                               places_counties_sf %>% 
#                               st_drop_geometry() %>% 
#                               pull(county_GEOID)),
#                    color = "white",
#                    col.regions = "blue",
#                    alpha.regions = 0.4) # ,
# url = paste0(getwd(), "/places_counties.html"))


# Export ------------------------------------------------------------------


# Places
write_csv(dict_places, "dictionaries/dict_places.txt")

# Tracts
st_write(tracts_sf, "dictionaries/geo_tract/geo_tract.shp", delete_dsn = TRUE)

# Places, counties, tracts crosswalk
write_csv(dict_location_crosswalk, "dictionaries/dict_location_crosswalk.txt")
