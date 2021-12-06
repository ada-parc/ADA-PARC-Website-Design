# dict_vars <- read_csv(here("dictionaries", "dict_vars_edits.csv"))
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

getUrbnGeo <- function(data, selected, quartiles = NULL, labels = NULL, interactive = T) {
  
  df <- get_urbn_map("territories_states", sf = TRUE) %>% 
    filter(!state_fips %in% c("60", "66", "69", "78")) %>% 
    select("ABBR" = state_abbv) %>% 
    inner_join(data %>% 
                 select(ABBR, !!sym(selected)) %>%
                 filter(!is.na(!!sym(selected))),
               by = "ABBR")
  
  if(interactive) {
    df %>%
      rowwise() %>% 
      mutate("hover_text" := ifelse(grepl("_pct$", 
                                          selected),
                                    paste0(round(!!sym(selected), 1), "%"),
                                    abbreviate_number(!!sym(selected))))
  } else if(!is.null(quartiles) & !is.null(labels)) {
    df %>%
      rowwise() %>% 
      mutate("quartile_fill" = cut(!!sym(selected), 
                                   breaks = quartiles, 
                                   labels = labels, 
                                   include.lowest = TRUE))
  } else {
    stop("Static plots require quartile and label values")
  }
  
}

makeTmapObject <- function(states_sf, selected, title = "", palette_selected) {
  tmap_object <- tm_shape(states_sf) +
    tm_basemap(NULL) +
    tm_polygons(
      col = selected,
      style = "quantile",
      n = 4,
      palette = palette_selected,
      popup.vars = c("Selected variable: " = "hover_text"),
      title = title,
      legend.format = list(fun = function(x) 
        if(grepl("_pct$", selected)) {
          paste0(round(x, 1), "%") }
        else { scales::comma(x) } )
    ) +
    # tm_shape(states_sf) +
    tm_borders(col = "black", lwd = 0.3) + 
    tm_layout(legend.stack = "horizontal") +
    tm_view(set.view = 3.5,
            leaflet.options = list(zoomSnap = 0.5,
                                   zoomDelta = 0.5),
            view.legend.position = c("left", "bottom"))
  
  tmap_object
}

makeGgplotObject <- function(states_sf, legend_title, palette_selected) {
  ggplot(states_sf) +
    geom_sf(aes(fill = quartile_fill),
            color = "black", size = 0.25) +
    scale_fill_brewer(palette = palette_selected,
                      na.value = "grey") +
    # Text
    geom_sf_text(data = get_urbn_labels(map = "territories_states", 
                                        sf = TRUE) %>% 
                   filter(!state_fips %in% c("60", "66", "69", "78")),
                 aes(label = state_abbv),
                 size = 4, fontface = "bold", check_overlap = TRUE) +
    # Labels
    labs(x = "", y = "", 
         # title = title,
         fill = legend_title) +
    theme_void() +
    # Theme, removes all of the grid elements that we don't need
    theme(plot.background = element_rect(colour = "white"), 
          panel.grid = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.spacing = unit(0L, "pt"),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", vjust = 0.75),
          legend.text = element_text(vjust = .5),
          legend.key = element_rect(color = "black"),
          strip.text.x = element_text(size = 9L),
          text = element_text(size = 16)) +
    guides(fill = guide_legend(label.position = "bottom"))
}

# rename, since this isn't exclusively for interactive maps
# probably renderNationalMap()
render_geo_interactive_map <- function(data, selected, 
                                       palette_selected = "YlOrBr") {
  
  if(!is.data.frame(data) & !is_tibble(data)) {
    stop("data must be a dataframe or tibble object")
  }
  
  if(!is.character(selected)) {
    stop("selected must be a character string")
  }
  
  category <- deparse(substitute(data)) # Need to ensure data is passed as a string of the object name
  
  legend_title <- paste0(dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1])

  if(!isCompVar(category, selected)){
    states_sf <- getUrbnGeo(data, selected)
    print("states_sf")
    makeTmapObject(states_sf, selected, legend_title, palette_selected)
    
  } else {

    comp_var <- getCompVar(category, selected)
    
    legend_title_comp <- paste0(dict_vars$var_pretty[which(dict_vars$var_readable == comp_var)][1])
    
    no_classes <- 4
    
    quartiles <- quantile(data %>% pull(!!sym(selected)), 
                          probs = seq(0, 1, length.out = no_classes + 1),
                          na.rm = TRUE)
    
    labels <- set_quartile_labels(quartiles, 4, selected)
    
    states_sf <- getUrbnGeo(data, selected, quartiles, labels, interactive = F)
    p1 <- makeGgplotObject(states_sf, legend_title, palette_selected)
    # print("p1")
    
    quartiles <- quantile(data %>% pull(!!sym(comp_var)), 
                          probs = seq(0, 1, length.out = no_classes + 1),
                          na.rm = TRUE)
    
    labels <- set_quartile_labels(quartiles, 4, comp_var)
    
    states_sf <- getUrbnGeo(data, comp_var, quartiles, labels, interactive = F)
    p2 <- makeGgplotObject(states_sf, legend_title_comp, palette_selected)
    # print("p2")
    
    grid.arrange(p1, p2, ncol = 2)
  }
  
}
render_geo_interactive_map(community_participation, "pwod_19_64_insured_pct")
# render_geo_interactive_map(demographics, "pop_total")
