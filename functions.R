

# MongoDB connection function ------------------------------------------------


# MongoDB connection using config.yml values
fun_mongo_connect <- function(host_name, collection_name, database_name){
  
  mongolite::mongo(collection = collection_name,
                   url = sprintf("mongodb+srv://%s:%s@%s/%s",
                                 config::get("user"),
                                 config::get("password"),
                                 host_name,
                                 database_name))
  
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
              wide = temp_wide)
      
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
                select(clean_state) %>% 
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
      #           select(clean_state) %>%
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
    pmap_df(.l = tracts_places_counties() %>% 
              select(STATEFP, COUNTYFP) %>% 
              distinct(),
            .f = ~(tigris::tracts(state = ..1,  
                                  county = ..2,
                                  cb = TRUE, 
                                  class = "sf") %>% 
                     select(GEOID) %>% 
                     st_join(tracts_places() %>%
                               select(place_GEOID),
                             left = FALSE)))
    
    # Static check
    # pmap_df(.l = tracts_places_counties %>%
    #           select(STATEFP, COUNTYFP) %>%
    #           distinct() %>%
    #           head(1),
    #         .f = ~(tigris::tracts(state = ..1,
    #                               county = ..2,
    #                               cb = TRUE,
    #                               class = "sf") %>%
    #                  select(GEOID) %>%
    #                  st_join(tracts_places %>%
    #                            select(place_GEOID),
    #                          left = FALSE)))
    
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
    mutate("table_name" = gsub( "_.*$", "", variable)) %>% 
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
