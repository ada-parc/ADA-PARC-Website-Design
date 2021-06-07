

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

# Plotting functions

render_tile_map <- function(data, selected) {
  
  # ### Unnecessary if selected is being passed as a string
  # # but we may want to pass the variable itself at some point
  # selected_varname <- deparse(substitute(!!sym(selected))) %>% gsub("^[^\\$]*\\$", "", .) 
  # ### Parsing the regex
  # # ^ anchor for beginning of string
  # # [^\\$]* matches everything excluding "$" char
  # # \\$ terminates match at "$" char
  # # example: deparse(substitute(national_demographic_readable$GEOID)) %>% gsub("^[^\\$]*\\$", "", .)
  # # outputs: "GEOID"
  
  # Perform the important transformations to variables based on the type of number
  if(grepl("pct", selected)) {
    fill_text <- geom_text(aes(label = paste0(round(!!sym(selected), 1), '%')),
              color = "white", size = 4)
  } else {
    fill_text <- geom_text(aes(label = paste0(scales::comma(round(!!sym(selected), -5)))), # rounds to nearest 100k
              color = "white", size = 4)
  }
  
  title <- dict_vars$national_dropdown_label[which(dict_vars$var_readable == selected)][1]
  
  data %>%
    ggplot(aes(x = 1, y = 1, # A tile map without x or y axis changes will fill out the tile for the state
               fill = !!sym(selected))) + # Selected variable
    geom_tile() + # Imports x and y values
    fill_text + 
    labs(x = "", y = "", title = title) +
    facet_geo(facets = ~ ABBR, grid = "us_state_with_DC_PR_grid2") +
    theme(plot.background = element_rect(colour = "white"), # Removes all of the grid elements that we don't need
          panel.grid = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.spacing = unit(0L, "pt"),
          legend.position = "none",
          strip.text.x = element_text(size = 9L)) +
    scale_fill_continuous(high = "#132B43", low = "#56B1F7") # reverses the default gradient direction so that dark blue is associated with larger values
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
