
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

# Create non-overlapping quartile buckets
create_non_overlapping_buckets <- function(data) {
  
  probs <- seq(0, 1, length.out = 5)
  
  quantiles <- quantile(data, probs, na.rm=TRUE)
  
  round_dynamic <- function(x, precision) {
    return(round(x * 10^precision) / 10^precision)
  }
  
  floor_dynamic <- function(x, precision) {
    return(floor(x * 10^precision) / 10^precision)
  }
  
  ceiling_dynamic <- function(x, precision) {
    return(ceiling(x * 10^precision) / 10^precision)
  }
  
  adjust_precision <- function(quantiles) {
    precision <- 0
    for (i in 2:(length(quantiles) - 1)) {
      while (round_dynamic(quantiles[i-1], precision) >= round_dynamic(quantiles[i], precision)) {
        precision <- precision + 1
      }
    }
    return(precision)
  }
  
  precision <- adjust_precision(quantiles)
  bounds <- sapply(quantiles, function(x) round_dynamic(x, precision))
  bounds[1] <- floor_dynamic(quantiles[1], precision)
  bounds[length(bounds)] <- ceiling_dynamic(quantiles[length(quantiles)], precision)
  
  return(bounds)
}

format_ranges <- function(breaks, col_name) {
  if (grepl("pct", col_name, ignore.case = TRUE)) {
    # Format as percentages
    
    if(any(breaks > 1)) {
      formatted_breaks <- breaks
    } else {

      formatted_breaks <- round(breaks * 100)
    }
    
    formatted_ranges <- paste0(head(formatted_breaks, -1), "%-", tail(formatted_breaks, -1), "%")
  } else {
    # Format with commas for large numbers
    formatted_breaks <- formatC(breaks, format = "f", big.mark = ",", digits = 0)
    formatted_ranges <- paste(head(formatted_breaks, -1), tail(formatted_breaks, -1), sep = "-")
  }
  return(formatted_ranges)
}

render_national_map <- function(category,
                                selected,
                                palette_selected = "YlOrBr",
                                output_asp_ratio = 0.45) {
  # Static check
  # category <- "is_community_participation"
  # selected <- "pwd_19_64_insured_public_pct"
  # palette_selected <- "YlOrBr"
  # output_asp_ratio <- 0.45
  
  # category <- "is_community_living"
  # selected <- "pwd_grpquarters_institution"
  
    if (!is.character(category)) {
      stop("category must be a character string")
    }
    if (!is.character(selected)) {
      stop("selected must be a character string")
    }
    
    # isCompVar
    display_type <- dict_vars %>%
      filter(var_readable == selected, !!sym(category)) %>%
      pull(display_type)
    
    is_comp <- ifelse(display_type == "comp",
                      TRUE, FALSE)
    
    palette <-
      brewer.pal(4, palette_selected)
    
    if (!is_comp) {
      
      legend_title <-
        paste0(dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1])
      
      us_states_with_data <- us_states %>%
        left_join(
          eval(sym(str_remove(category, "^is_"))) %>%
            filter(ABBR != "USA") %>%
            select(NAME, estimate = sym(selected)) %>%
            mutate(estimate = as.numeric(
              gsub(
                pattern = "[,]",
                replacement = "",
                x = estimate
              )
            ))
        )

      # Calculate quantile breaks and create custom labels

      buckets <- create_non_overlapping_buckets(us_states_with_data$estimate)
      
      labels <- format_ranges(buckets, selected)

      us_states_with_data <- us_states_with_data  %>% 
        mutate(estimate_cat = cut(
          estimate,
          breaks = buckets,
          include.lowest = TRUE,
          labels = labels))
      
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

      base_var <- dict_vars %>%
        filter(var_readable == selected, !!sym(category)) %>%
        pull(var_base)
      
      comp_var <- dict_vars %>%
        filter(var_base == base_var, var_readable != selected) %>%
        pull(var_readable)
      
      us_states_with_data <- us_states %>%
        left_join(
          eval(sym(str_remove(category, "^is_"))) %>%
            filter(ABBR != "USA") %>%
            select(
              STUSPS = ABBR,
              estimate = sym(selected),
              estimate_2 = sym(comp_var)
            ) %>%
            mutate(estimate = as.numeric(
              gsub(
                pattern = "[,]",
                replacement = "",
                x = estimate
              )
            ),
            estimate_2 = as.numeric(
              gsub(
                pattern = "[,]",
                replacement = "",
                x = estimate_2
              )
            )))
      
      # Combine PWD and PWOD
      combined_var <- c(us_states_with_data$estimate, us_states_with_data$estimate_2)
      
      breaks <- create_non_overlapping_buckets(combined_var)
      labels <- format_ranges(breaks, selected)
      
      us_states_with_data <- us_states_with_data %>% 
        mutate(
        estimate_cat = factor(
          cut(
            estimate,
            breaks = breaks,
            include.lowest = TRUE,
            labels = labels
          ),
          levels = labels
        ),
        estimate_2_cat = factor(
          cut(
            estimate_2,
            breaks = breaks,
            include.lowest = TRUE,
            labels = labels
          ),
          levels = labels
        )
      )
      
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
              label.position = "bottom",
              label.hjust = 0.5,
              nrow = 1
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
