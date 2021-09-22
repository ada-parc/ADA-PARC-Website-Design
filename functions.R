

# MongoDB functions ------------------------------------------------


# MongoDB connection using config.yml values
fun_mongo_connect <- function(host_name, collection_name, database_name){
  
  url <- sprintf("mongodb+srv://%s:%s@%s/%s",
                 config::get("user"),
                 config::get("password"),
                 config::get(host_name),
                 database_name)
  
  mongolite::mongo(collection = collection_name,
                   url = url,
                   options = ssl_options(weak_cert_validation = TRUE))
  
}

# MongoDB download table 
# TODO: Change these for loops into map/apply function calls
fun_pull_mongo_data <- function(tables, host_name = "host_prod", geo = F) { 
  
  if (geo != F) { # Generally this function will be used to pull data from geographic tables
    for (t in seq_along(tables)) {
      # print(paste("Connection to:", paste0("acs_", geo, "_", tables[t])))
      temp_mongo_conn <- fun_mongo_connect(collection_name = paste0("acs_", geo, "_", tables[t]),
                                           database_name = "ADA-PARC",
                                           host_name = host_name)
      
      # Creates one temp_df object per value in tables
      assign(paste0("temp_df", t), temp_mongo_conn$find())
    }
    # joins all of the temp_dfs into one large table
    df <- reduce(mget(ls(pattern = "temp_df")), 
                 left_join, by = c("GEOID", "NAME"))
  } else { # But sometimes it needs to be used to pull dictionaries or other data
    temp_mongo_conn <- fun_mongo_connect(collection_name = tables, # tables should be something like "vars_dict" or "variable_lu"
                                         database_name = "ADA-PARC",
                                         host_name = host_name)
    
    df <- temp_mongo_conn$find()
    
    return(df)
  }
}


# Dashboard functions -----------------------------------------------------

# Set quartiles
set_quartile_labels <- function(quartiles, no_classes, selected) {

  labels <- c()
  
  # Custom labels based on percent or value
  for(idx in 1:length(quartiles)){
    if(grepl("pct", selected)) {
      
      # Percent, add divide by 100, add symbol to text
      labels <- c(labels, paste0(scales::percent(quartiles[idx] / 100), 
                                 "-", 
                                 scales::percent(quartiles[idx + 1] / 100)))
      
    } else {
      
      # Values
      labels <- c(labels, paste0(scales::comma(quartiles[idx]), 
                                 "-", 
                                 scales::comma(quartiles[idx + 1])))
    }
  }
  # Remove last label which will have NA
  labels <- labels[1:length(labels)-1]
}

# Abbreviates values for large numbers in render_tile_map
abbreviate_number <- function(x)
{
  x <- x / 1000000
  # print(x)
  
  if (x >= 1) {
    return(paste0(round(x, 1), "M"))
  } else {
    x <- x * 1000
    return(paste0(round(x, 0), "K"))
  }
}

abbreviate_number <- Vectorize(abbreviate_number) # Must be vectorized to provide correct values in text generation

# Tile map function
render_tile_map <- function(data, selected, palette_selected) {

  
  no_classes <- 4
  
  quartiles <- quantile(data %>% filter(!is.na(!!sym(selected))) %>% pull(!!sym(selected)), 
                        probs = seq(0, 1, length.out = no_classes + 1))
  
  labels <- set_quartile_labels(quartiles, no_classes, selected)
  
  # Set tile text fill based on contrast
  hcl <- farver::decode_colour(RColorBrewer::brewer.pal(no_classes, 
                                                        palette_selected), 
                               "rgb", "hcl")
  
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
  
  # Tile text fill
  if(grepl("pct", selected)) {
    
    # Percents, adds symbol to text
    fill_text <- geom_text(aes(
      label = paste0(round(!!sym(selected), 1), "%"),
      color = quartile_fill),
      size = 4,
      show.legend = FALSE)
    
  } else {
    
    # Values, rounds to nearest 100k
    fill_text <- geom_text(aes(
      label = abbreviate_number(!!sym(selected)),
      color = quartile_fill),
      size = 4,
      show.legend = FALSE)
    
  }
  
  # Set map title and legend
  title <- dict_vars$national_dropdown_label[which(dict_vars$var_readable == selected)][1]
  legend_title <- dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1]
  
  # Plot tile map
  data %>%
    filter(!is.na(!!sym(selected))) %>%
    mutate("quartile_fill" = cut(data %>% filter(!is.na(!!sym(selected))) %>% pull(!!sym(selected)), 
                                 breaks = quartiles, 
                                 labels = labels, 
                                 include.lowest = TRUE)) %>% 
    ggplot(aes(x = 1, y = 1, # A tile map without x or y axis changes will fill out the tile for the state
               fill = quartile_fill)) + # Selected variable
    geom_tile() + # Imports x and y values
    # Fill
    scale_fill_brewer(palette = palette_selected) +
    # Text
    fill_text +
    scale_color_manual(values = label_col) +
    # Labels
    labs(x = "", y = "", 
         # title = title,
         fill = legend_title) +
    # Facet
    facet_geo(facets = ~ ABBR, grid = "us_state_with_DC_PR_grid2") +
    # Theme, removes all of the grid elements that we don't need
    theme(
      plot.background = element_rect(colour = "white"), 
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
      plot.margin = unit(c(0,0,2,0), "cm"),
      strip.text.x = element_text(size = 9L)
    ) +
    guides(fill = guide_legend(label.position = "bottom"))
  
}

# Geographic map function
render_geo_interactive_map <- function(data, selected, palette_selected) {
  
  no_classes <- 4
  
  quartiles <- quantile(data %>% filter(!is.na(!!sym(selected))) %>% pull(!!sym(selected)), 
                        probs = seq(0, 1, length.out = no_classes + 1))
  
  labels <- set_quartile_labels(quartiles, no_classes, selected)
  
  # Set map title and legend
  title <- dict_vars$national_dropdown_label[which(dict_vars$var_readable == selected)][1]
  legend_title <- paste0(dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1], ": ")
  
  # US State geography, remove territories, join data
  states_sf <- get_urbn_map("territories_states", sf = TRUE) %>% 
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
  
  # Plot geographic map
  tmap_mode("view")
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
  
  tmap_object +
    tm_view(set.view = 3.75,
            leaflet.options = list(zoomSnap = 0.5,
                                   zoomDelta = 0.5))
  
}

# Geographic map function
render_geo_static_map <- function(data, selected, palette_selected) {
  
  # Set quartiles
  no_classes <- 4
  labels <- c()
  quartiles <- quantile(data %>% pull(!!sym(selected)), 
                        probs = seq(0, 1, length.out = no_classes + 1))
  
  # Custom labels based on percent or value
  for(idx in 1:length(quartiles)){
    if(grepl("pct", selected)) {
      
      # Percent, add divide by 100, add symbol to text
      labels <- c(labels, paste0(scales::percent(quartiles[idx] / 100), 
                                 "-", 
                                 scales::percent(quartiles[idx + 1] / 100)))
      
    } else {
      
      # Values
      labels <- c(labels, paste0(scales::comma(quartiles[idx]), 
                                 "-", 
                                 scales::comma(quartiles[idx + 1])))
    }
  }
  # Remove last label which will have NA
  labels <- labels[1:length(labels)-1]
  
  # Set map title and legend
  title <- dict_vars$national_dropdown_label[which(dict_vars$var_readable == selected)][1]
  legend_title <- paste0(dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1], ": ")
  
  # US State geography, remove territories, join data
  states_sf <- get_urbn_map("territories_states", sf = TRUE) %>% 
    filter(!state_fips %in% c("60", "66", "69", "78")) %>% 
    select("ABBR" = state_abbv) %>% 
    inner_join(data %>% 
                 select(ABBR, !!sym(selected)) %>%
                 filter(!is.na(!!sym(selected))),
               by = "ABBR") %>% 
    rowwise() %>% 
    mutate("hover_text" := ifelse(grepl("_pct$", 
                                        selected),
                                  paste0(ABBR, ":\n", 
                                         round(!!sym(selected), 1), "%"),
                                  paste0(ABBR, ":\n", 
                                         abbreviate_number(!!sym(selected)))),
           "quartile_fill" = cut(!!sym(selected), 
                                 breaks = quartiles, 
                                 labels = labels, 
                                 include.lowest = TRUE))
  
  # Plot geographic map
  ggplot_object <- ggplot(states_sf) +
    geom_sf(aes(fill = quartile_fill),
            color = "black", size = 0.25) +
    scale_fill_brewer(palette = palette_selected) +
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
  
  ggplot_object
  
}


# census_download.Rmd functions -----------------------------------------------


# Function for ACS download
fun_download_acs_data <- function(geo) {
  
  
  ## ----- Define intermediate variables -----

  
  # Year
  temp_year <- reactive({
    
    case_when(geo == "state" ~
                as.numeric(input$states_acs_year),
              geo == "place" ~
                as.numeric(input$cities_acs_year),
              TRUE ~ 2019)
    
  })
  
  message(paste0("Year = ", as.character(temp_year())))
  
  # Survey
  temp_survey <- reactive({
    
    case_when(geo == "state" ~
                coalesce(str_extract(input$states_acs_dataset,
                                     pattern = "^.*(?=/)"),
                         input$states_acs_dataset),
              geo == "place" ~
                coalesce(str_extract(input$cities_acs_dataset,
                                     pattern = "^.*(?=/)"),
                         input$cities_acs_dataset),
              TRUE ~ "acs5")
    
  })
  
  message(paste0("Survey = ", temp_survey()))
  
  # ACS variables
  acs_vars_selected <- reactive({ 
    
    if (geo == "state") {
      states_acs_vars_selected() %>% 
        pull(name)
    } else if (geo == "place") {
      cities_acs_vars_selected() %>% 
        pull(name)
    } else if (geo == "tract"){
      states_acs_vars_selected() %>% 
        pull(name)
    } else {
      states_acs_vars_selected() %>% 
        pull(name)
    }
    
  })
  
  # Geometry
  temp_geometry <- case_when(geo %in% c("state", "place") ~
                               FALSE,
                             TRUE ~ FALSE)
  
  message(paste0("Geometry = ", as.character(temp_geometry)))
  
  # Wide
  temp_wide <- case_when(geo == "state" ~
                           TRUE,
                         TRUE ~ TRUE)
  
  message(paste0("Wide = ", as.character(temp_wide)))
  
  # We want state abbreviations in the data
  if(geo == "state"){
    state_id <- tibble(NAME = state.name,
                       ABBR = state.abb) %>%
      # Add DC and Puerto Rico values to ABBR
      bind_rows(
        tibble(NAME = c("District of Columbia", "Puerto Rico"),
                       ABBR = c("DC", "PR"))
        )
  }
  
  ## ----- Download ACS data -----
  
  # Grab from API 
  message(paste0("Starting API Call"))
  
  acs_raw <- reactive({
    
    if (geo == "state"){
      
      get_acs(geography = geo,
              year = temp_year(),
              variables = acs_vars_selected(),
              survey = temp_survey(),
              geometry = temp_geometry,
              wide = temp_wide) %>%
        left_join(state_id, by = "NAME") %>%
        relocate(ABBR, .after = "NAME")
      
      # Static check (state)
      # get_acs(geography = "state",
      #         year = 2019,
      #         variables = states_acs_vars_selected() %>% pull(name),
      #         survey = "acs5",
      #         geometry = temp_geometry,
      #         wide = temp_wide)
      
    } else if (geo == "place"){ 
      
      # Place data has to be pulled by state
      # Mapped by state FIP, filter to selected cities
      pmap_df(.l = cities_acs_places_selected() %>% 
                mutate("STATEFP" = str_sub(GEOID, 1, 2)) %>% 
                select(STATEFP) %>% 
                distinct(),
              .f = ~(get_acs(geography = geo,
                             year = temp_year(),
                             variables = acs_vars_selected(),
                             survey = temp_survey(),
                             state = ..1, 
                             geometry = temp_geometry,
                             wide = temp_wide) %>% 
                       semi_join(cities_acs_places_selected(), 
                                 by = "GEOID")))
      
      # Static check
      # pmap_df(.l = cities_acs_places_selected %>%
      #           mutate("STATEFP" = str_sub(GEOID, 1, 2)) %>% 
      #           select(STATEFP) %>%
      #           distinct(),
      #         .f = ~(get_acs(geography = "place",
      #                        year = 2019,
      #                        variables = c("S0101_C01_006",
      #                                      "S0101_C01_007",
      #                                      "S0101_C01_008",
      #                                      "S0102_C01_006"),
      #                        survey = "acs5",
      #                        state = ..1,
      #                        geometry = FALSE,
      #                        wide = TRUE) %>%
      #                  semi_join(cities_acs_places_selected,
      #                            by = "GEOID")))
      
    } else if (geo == "tract"){ 
      
      # Tract data has to be pulled by state and county
      # Mapped by state and county FIPs, filter to selected cities
      # Summary variable --> total population
      pmap_df(.l = tracts_places_counties() %>%
                mutate("STATEFP" = str_sub(county_GEOID, 1, 2),
                       "COUNTYFP" = str_sub(county_GEOID, 3, 5)) %>% 
                select(STATEFP, COUNTYFP) %>% 
                distinct(),
              .f = ~(get_acs(geography = geo,
                             year = temp_year(),
                             variables = acs_vars_selected(),
                             survey = temp_survey(),
                             state = ..1, 
                             county = ..2,
                             geometry = temp_geometry,
                             wide = temp_wide)))
      
      # Static check
      # pmap_df(.l = tracts_places_counties %>%
      #           mutate("STATEFP" = str_sub(county_GEOID, 1, 2),
      #                  "COUNTYFP" = str_sub(county_GEOID, 3, 5)) %>% 
      #           select(STATEFP, COUNTYFP) %>%
      #           distinct(),
      #         .f = ~(get_acs(geography = "tract",
      #                        year = 2019,
      #                        variables = c("S0101_C01_006",
      #                                      "S0101_C01_007",
      #                                      "S0101_C01_008"),
      #                        survey = "acs5",
      #                        state = ..1,
      #                        county = ..2,
      #                        geometry = FALSE,
      #                        wide = TRUE)))
      
    }
    
  })
  
  
  ## ----- Grab tract geography (optional) -----
  
  
  # Tracts SF dataframe
  tracts_geography <- reactive({
    
    # Require tract input
    req(geo == "tract")
    
    # Download tract geographies, filter using places
    # Temp files
    temp <- tempfile()
    tempd <- tempdir()
    
    # Download and unzip shapefile
    download.file("https://github.com/sean-connelly/ADA-PARC-Website-Design/raw/national-tile-map/dictionaries/geo_tract.zip", # TODO: Host this on MongoDb instead of github
                  destfile = temp,
                  mode = "wb")
    unzip(temp, exdir = tempd)
    
    # Filter
    st_read(paste0(tempd, "/geo_tract/geo_tract.shp")) %>% 
      select("GEOID" = t_GEOID, 
             "NAME" = tr_NAME) %>% 
      filter(GEOID %in% tracts_places_counties()$tract_GEOID)
    
    # # Static check
    # # Temp files
    # temp <- tempfile()
    # tempd <- tempdir()
    # 
    # # Download and unzip shapefile
    # download.file("https://github.com/sean-connelly/ADA-PARC-Website-Design/raw/national-tile-map/dictionaries/geo_tract.zip",
    #               destfile = temp,
    #               mode = "wb")
    # unzip(temp, exdir = tempd)
    # 
    # # Filter
    # st_read(paste0(tempd, "/geo_tract/geo_tract.shp")) %>%
    # select("tract_GEOID" = t_GEOID, 
    #        "tract_NAME" = tr_NAME) %>% 
    #   filter(GEOID %in% tracts_places_counties$tract_GEOID)
    # 
    # # Remove temp files
    # unlink(temp)
    # unlink(tempd)
    
  })
  
  # Filter, include tract geographies if needed
  acs_output <- reactive({
    
    if (geo == "tract"){
      
      # Restrict tabular data using filtered tracts
      # Return list with [[1]] = data, [[2]] = geography
      list("data" = acs_raw() %>% 
             filter(GEOID %in% tracts_geography()$GEOID),
           "geography" = tracts_geography())
      
    } else { 
      
      # Otherwise return raw result
      acs_raw()
      
    }
    
  })
  
  return(acs_output())
  
}

# Organize data into list of dfs and export
fun_write_by_geo_table <- function(df, host_name, geo) {
  
  
  ## ----- Prep data for MongoDB insertion ----- 

  
  # Group by table name
  df <- df %>% 
    mutate("table_name" = str_extract(variable, "^[^_]*")) %>%
    group_by(table_name) %>% 
    drop_na(estimate) %>% 
    distinct()
  
  # Get table names to rename list elements 
  df_group_names <- group_keys(df) %>% 
    mutate("table_name" = paste0("acs_", geo, "_", table_name))
  
  # Reconfigure into list of dfs for MongoDB
  df_list <- df %>% 
    group_map( ~{ .x %>% 
        pivot_wider(names_from = variable, 
                    names_glue = "{variable}_{.value}",
                    values_from = c(estimate, moe)) },
        .keep = FALSE)
  
  # Set names for list
  df_list <- df_list %>% 
    setNames(df_group_names$table_name)
  
  
  ## ----- Write to MongoDB , loop over table names -----

  
  # Iterate over list of dataframes
  map2(.x = df_list,
       .y = names(df_list),
       .f = ~{
         # Connect to MongoDB database/collection
         mongo_conn <- fun_mongo_connect(host_name = host_name,
                                         collection_name = .y,
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

# Custom function for download button in Rmd
downloadButtonRmd <- function (outputId, label = "Download", class = NULL, ...){
  tags$a(id = outputId, 
         class = paste("btn btn-default shiny-download-link", class), 
         href = "", target = "_blank", download = NA, 
         icon("download"), label, ...)
}


# Accessibility Functions -------------------------------------------------


englishLangList <- function(x) {
  if(length(x) > 2){
    next_to_last <- length(x) - 1
    paste0(paste(x[1:next_to_last], collapse = ", "), ", and ", x[length(x)], collapse = "")
  } else {
    paste(x, collapse = " and ")
  }
}

between <- function(df, variable, probs) {
  df %>% 
    filter(!!sym(variable) >= probs[1] & !!sym(variable) <= probs[2]) %>%
    pull(NAME)
}


altText <- function(data, variable) {
  
  # Selected data, format min/max for summary
  df <- data %>%
    select(NAME, ABBR, sym(variable)) %>%
    filter(!is.na(sym(variable)))
  
  # Min
  text_min <- data %>%
    mutate("State" = paste0(NAME, " (", ABBR, ")")) %>% 
    select(State, sym(variable)) %>%
    filter(!!sym(variable) == min(!!sym(variable))) %>% 
    slice(1) %>% 
    mutate(across(-State & -ends_with("_pct"),
                  ~scales::comma(.x))) %>% 
    mutate(across(ends_with("_pct"),
                  ~scales::percent(.x, 
                                   accuracy = 0.1,
                                   scale = 1))) %>% 
    mutate("summary_text" = paste0(" The lowest state or territory was ",
                                   State, " at ", 
                                   !!sym(variable), ".")) %>% 
    pull(summary_text)
    
  # Max
  text_max <- data %>%
    mutate("State" = paste0(NAME, " (", ABBR, ")")) %>% 
    select(State, sym(variable)) %>%
    filter(!!sym(variable) == max(!!sym(variable))) %>% 
    slice(1) %>% 
    mutate(across(-State & -ends_with("_pct"),
                  ~scales::comma(.x))) %>% 
    mutate(across(ends_with("_pct"),
                  ~scales::percent(.x, 
                                   accuracy = 0.1,
                                   scale = 1))) %>% 
    mutate("summary_text" = paste0(" The highest state or territory was ",
                                   State, " at ", 
                                   !!sym(variable), ".")) %>% 
    pull(summary_text)
  
  # Max static check
  # max_text_static <- demographics %>%
  #   mutate("State" = paste0(NAME, " (", ABBR, ")")) %>%
  #   select(State, sym("pop_total")) %>%
  #   filter(!!sym("pop_total") == max(!!sym("pop_total"))) %>%
  #   mutate(across(-State & -ends_with("_pct"),
  #                 ~scales::comma(.x))) %>%
  #   mutate(across(ends_with("_pct"),
  #                 ~scales::percent(.x,
  #                                  accuracy = 0.1,
  #                                  scale = 1))) %>%
  #   mutate("summary_text" = paste0(" The highest state was ",
  #                                  State, " at ",
  #                                  !!sym("pop_total"), ".")) %>%
  #   pull(summary_text)
  
  # Title, vars_pretty field for variable
  title <- dict_vars %>% 
    filter(!is.na(national_dropdown_label),
           var_readable == sym(variable)) %>% 
    head(1) %>% 
    pull(national_dropdown_label)
  
  # Summary text for variable
  summary_text <- dict_vars %>% 
    filter(!is.na(national_dropdown_label),
           var_readable == sym(variable)) %>% 
    head(1) %>% 
    pull(national_summary_text)
  
  # Text for summary
  paste0("<b>", title, "</b><br>",
         summary_text, " ",
         # Min/Max
         text_min, text_max
         )
  
}
