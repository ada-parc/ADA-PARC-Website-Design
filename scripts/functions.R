
# Dashboard functions -----------------------------------------------------

altTitle <- function(variable) {
  
  # Title, vars_pretty field for variable
  title <- dict_vars %>% 
    filter(!is.na(national_dropdown_label),
           var_readable == sym(variable)) %>% 
    head(1) %>% 
    pull(var_pretty)
  
  title <- str_trim(str_replace_all(title,
                                    " (with|without) Disabilities", ""))
  
  title <- str_trim(str_replace_all(title,
                                    " (with|without) Disability", ""))
  
  return(title)
  
}


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
  rtn_labels <- labels[1:length(labels)-1]
  return(rtn_labels)
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

abbreviate_number <- Vectorize(abbreviate_number) # Must be vectorized to perform operation row-wise

render_national_map <- function(category,
                                selected,
                                palette_selected = "YlOrBr",
                                output_asp_ratio = 0.45) {
  # Static check
  # category <- "is_community_participation"
  # selected <- "pwd_19_64_insured_public_pct"
  # palette_selected <- "YlOrBr"
  # output_asp_ratio <- 0.45
  
    if (!is.character(category)) {
      stop("category must be a character string")
    }
    if (!is.character(selected)) {
      stop("selected must be a character string")
    }
    
    data_for_states <- eval(sym(str_remove(category, "^is_"))) %>%
      filter(ABBR != "USA")
    
    variable_column <- data_for_states %>% pull(!!sym(selected))
    
    if (class(variable_column) == "character") {
      # remove all commas and decimals from a number, cast as numeric.
      
      variable_column <- as.numeric(gsub("[,]", "", variable_column))
      
    }
    
    # isCompVar
    display_type <- dict_vars %>%
      filter(var_readable == selected, !!sym(category)) %>%
      pull(display_type)
    
    is_comp <- ifelse(display_type == "comp",
                      TRUE, FALSE)
    
    variable_dataset <- data_for_states %>%
      select(NAME, estimate = sym(selected))
    
    if (!is_comp) {
      
      legend_title <-
        paste0(dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1])
      
      us_states_with_data <- us_states %>%
        left_join(variable_dataset)
      
      # Calculate quantile breaks and create custom labels
      breaks <-
        quantile(us_states_with_data$estimate,
                 probs = seq(0, 1, length.out = 5),
                 na.rm = TRUE)
      labels <-
        paste0(round(breaks[-length(breaks)]), "%-", round(breaks[-1]), "%")
      

      us_states_with_data <- us_states_with_data  %>% 
        mutate(estimate_cat = cut(
          estimate,
          breaks = breaks,
          include.lowest = TRUE,
          labels = labels))
      
      
      palette <-
        brewer.pal(4, palette_selected) # Change to "YlOrRd" or "GnBu" as needed
      
      # Create the map
      ggplot(data = us_states_with_data) +
        geom_sf(aes(fill = estimate_cat)) +
        geom_text(data = us_states_with_data,
                  aes(label = STUSPS,
                      x = x_lab,
                      y = y_lab)) +
        geom_segment(data = us_states_with_data %>%
                       filter(STUSPS %in% east_coast_states_to_relocate), 
                     aes(X, Y, xend = x_lab - 100000, yend = y_lab)) +
        scale_fill_manual(values = palette, name = legend_title) +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(size = 14, hjust = 0.5),
          legend.text = element_text(size = 10),
          legend.title.align = 0.5,
          legend.box = "horizontal",
          legend.box.just = "center",
        ) +
        guides(
          fill = guide_legend(
            title.position = "top",
            title.hjust = 0.5,
            label.position = "bottom",
            # Move labels below the legend keys
            label.hjust = 0.5,
            # Center the labels below the legend keys
            nrow = 1 # Ensure the legend items are in a single row
          )
        )
      
      
    } else {
      # side-by-side plots/comp (plot_1, plot_2)
      # Define variables
      
      base_var <- dict_vars %>%
        filter(var_readable == selected, !!sym(category)) %>%
        pull(var_base)
      
      comp_var <- dict_vars %>%
        filter(var_base == base_var, var_readable != selected) %>%
        pull(var_readable)
      
      variable_dataset <- data_for_states %>%
        select(STUSPS = ABBR,
               estimate = sym(selected),
               estimate_2 = sym(comp_var)) %>%
        mutate(estimate = as.numeric(gsub(
          pattern = "[,]",
          replacement = "",
          x = estimate
        )),
        estimate_2 = as.numeric(gsub(
          pattern = "[,]",
          replacement = "",
          x = estimate_2
        )))
      
      # Combine PWD and PWOD
      combined_var <- c(variable_dataset$estimate, variable_dataset$estimate_2)
      
      breaks <- quantile(combined_var,
                            probs = seq(0, 1, length.out = 5),
                            na.rm = TRUE)
      
      labels <- set_quartile_labels(breaks, 4, base_var)
      
      # Map title reworking
      legend_title_comp <-
        paste0(dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1])
      
      legend_title_comp <-
        str_trim(str_replace_all(legend_title_comp,
                                 " (with|without) Disabilities", ""))
      
      legend_title_comp <-
        str_trim(str_replace_all(legend_title_comp,
                                 " (with|without) Disability", ""))
      
      plot_1_title <- "People with Disabilities"
      plot_2_title <- "People without Disabilities"
      
      us_states_with_data <- us_states %>%
        left_join(variable_dataset) %>%
        mutate(
          estimate_cat = factor(cut(
            estimate,
            breaks = breaks,
            include.lowest = TRUE,
            labels = labels
          ), levels = labels),
          estimate_2_cat = factor(cut(
            estimate_2,
            breaks = breaks,
            include.lowest = TRUE,
            labels = labels
          ), levels = labels)
        )
      
      palette <- brewer.pal(4, "YlOrBr")
      

      map1 <- ggplot(data = us_states_with_data) +
        geom_sf(aes(fill = estimate_cat)) +
        geom_text(data = us_states_with_data,
                  aes(label = STUSPS,
                      x = x_lab,
                      y = y_lab)) +
        geom_segment(data = us_states_with_data %>%
                       filter(STUSPS %in% east_coast_states_to_relocate), 
                     aes(X, Y, xend = x_lab - 100000, yend = y_lab)) +
        scale_fill_manual(values = palette, drop = FALSE) +
        ggtitle(plot_1_title) +
        theme_void() +
        theme(
          legend.position = "none" # Hide legend for the first map
        )
      
      # Create the second map for 'estimate_2'
      map2 <- ggplot(data = us_states_with_data) +
        geom_sf(aes(fill = estimate_2_cat)) +
        geom_text(data = us_states_with_data,
                  aes(label = STUSPS,
                      x = x_lab,
                      y = y_lab)) +
        geom_segment(data = us_states_with_data %>%
                       filter(STUSPS %in% east_coast_states_to_relocate), 
                     aes(X, Y, xend = x_lab - 100000, yend = y_lab)) +
        scale_fill_manual(values = palette, drop = FALSE) +
        ggtitle(plot_2_title) +
        theme_void() +
        theme(
          legend.position = "none" # Hide legend for the second map
        )
      
      
      # Extract the legend from one of the maps
      legend <- cowplot::get_legend(
        ggplot(data = us_states_with_data) +
          geom_sf(aes(fill = estimate_cat)) +
          # geom_sf(aes(fill = estimate_2_cat)) +
          scale_fill_manual(values = palette, name = legend_title_comp, drop = FALSE) +
          theme_void() +
          theme(
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(size = 10, hjust = 0.5),
            legend.text = element_text(size = 8),
            legend.title.align = 0.5,
            legend.box = "horizontal",
            legend.box.just = "center"
          ) +
          guides(
            fill = guide_legend(
              title.position = "top",
              title.hjust = 0.5,
              label.position = "bottom", # Move labels below the legend keys
              label.hjust = 0.5, # Center the labels below the legend keys
              nrow = 1 # Ensure the legend items are in a single row
            )
          )
      )
      
      # Combine the maps and the legend using patchwork
      combined <- (map1 + map2) / legend + plot_layout(heights = c(10, 1))
      
      combined
    }
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
    filter(ABBR != "USA")

  # Min
  text_min <- df %>%
    mutate("State" = paste0(NAME, " (", ABBR, ")")) %>% 
    select(State, sym(variable)) %>%
    filter(!is.na(!!sym(variable))) %>% 
    filter(!!sym(variable) == min(!!sym(variable))) %>% 
    slice(1) %>% 
    mutate(across(ends_with("_pct"),
                  ~scales::percent(.x, 
                                   accuracy = 0.1,
                                   scale = 1))) %>% 
    mutate("summary_text" = paste0(" The lowest state or territory was ",
                                   State, " at ", 
                                   !!sym(variable), ".")) %>% 
    pull(summary_text)
    
  # Max
  text_max <- df %>%
    mutate("State" = paste0(NAME, " (", ABBR, ")")) %>% 
    select(State, sym(variable)) %>%
    filter(!is.na(!!sym(variable))) %>% 
    filter(!!sym(variable) == max(!!sym(variable))) %>% 
    slice(1) %>% 
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
  paste0(
    # "<b>", title, "</b><br>",
    if (is.na(summary_text)) {
      ""
    } else {
    paste0(summary_text, " ")
      }
    ,
    # Min/Max
    text_min, text_max
  )
  
}
