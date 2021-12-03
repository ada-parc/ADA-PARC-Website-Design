dict_vars <- read_csv(here("dictionaries", "dict_vars_edits.csv"))
tmap_mode("view")

# # Test configs
# # comp_var <- "pwd_18_64"
# # national_category_selector <- "is_demographics"
# 
# comp_var <- "pwod_19_64_insured_pct"
# national_category_selector <- "is_community_participation"
# 
# # comp_var <- ""
# # national_category_selector <- ""
# 
# 
# base_var <- str_extract(comp_var, "((?<=pwd_)|(?<=pwod_)).*$")


# Create a var_base value
# dict_vars <- dict_vars %>%
#   mutate(var_base = ifelse(display_type == "comp", str_extract(var_readable, "((?<=pwd_)|(?<=pwod_)).*$"), NA))

getCompVar <- function(category, topic) {
  if(!exists("dict_vars")) {
    stop("dict_vars not loaded")
  }
  
  # Idea is to go away from data, selected in this use case and just ensure it's passed strings to symbolize
  national_category_selector <- paste0("is_", category)

  if(!isCompVar(category, topic)) 
  {
    stop("Topic variable passed has no comparable")
  } else {
    base_var <- dict_vars %>% 
      filter(var_readable == topic, !!sym(national_category_selector)) %>% 
      pull(var_base)
    
    dict_vars %>%
      filter(var_base == base_var, var_readable != topic) %>%
      pull(var_readable)
  }
  
}

isCompVar <- function(category, topic) {
  national_category_selector <- paste0("is_", category)
  
  display_type <- dict_vars %>% 
    filter(var_readable == topic, !!sym(national_category_selector)) %>% 
    pull(display_type)
  
  ifelse(
    display_type == "comp",
    T,
    F
  )
}

getUrbnGeo <- function(data, selected) {
  return( 
    get_urbn_map("territories_states", sf = TRUE) %>% 
      filter(!state_fips %in% c("60", "66", "69", "78")) %>% 
      select("ABBR" = state_abbv) %>% 
      inner_join(data %>% 
                   select(ABBR, !!sym(selected)) %>%
                   filter(!is.na(!!sym(selected))),
                 by = "ABBR") %>% 
      rowwise() %>% 
      mutate("hover_text" := ifelse(grepl("_pct$", 
                                          selected),
                                    paste0(round(!!sym(selected), 1), "%"),
                                    abbreviate_number(!!sym(selected))))
  )
}

makeTmapObject <- function(states_sf, selected, palette_selected) {
  tmap_object <- tm_shape(states_sf) +
    tm_basemap(NULL) +
    tm_polygons(col = selected,
                style = "quantile",
                n = 4,
                palette = palette_selected,
                popup.vars = c("Selected variable: " = "hover_text"),
                title = "",
                legend.format = list(fun = function(x) 
                  if(grepl("_pct$", selected)) {
                    paste0(round(x, 1), "%") }
                  else { scales::comma(x) } )) +
    tm_shape(states_sf) +
    tm_borders(col = "black", lwd = 0.3)
}

render_geo_interactive_map <- function(data, selected, 
                                       palette_selected = "YlOrBr") {
  
  if(!is.data.frame(data) & !is_tibble(data)) {
    stop("data must be a dataframe or tibble object")
  }
  
  if(!is.character(selected)) {
    stop("selected must be a character string")
  }
  
  category <- deparse(substitute(data)) # Need to ensure data is passed as a string of the object name

  if(!isCompVar(category, selected)){
    states_sf <- getUrbnGeo(data, selected)
    
    p1 <- makeTmapObject(states_sf, selected, palette_selected)
    
    p1 + 
      tm_view(set.view = 3.5,
              leaflet.options = list(zoomSnap = 0.5,
                                     zoomDelta = 0.5))
  } else {

    comp_var <- getCompVar(category, selected)
    
    states_sf <- getUrbnGeo(data, selected)
    p1 <- makeTmapObject(states_sf, selected, palette_selected)
    
    states_sf <- getUrbnGeo(data, comp_var)
    p2 <- makeTmapObject(states_sf, comp_var, palette_selected)
    
    tmap_arrange(p1, p2, ncol = 2, sync = T)
  }
  
}
render_geo_interactive_map(community_participation, "pwod_19_64_insured_pct")
render_geo_interactive_map(demographics, "pop_total")
