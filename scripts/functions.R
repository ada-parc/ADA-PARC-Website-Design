
# Dashboard functions -----------------------------------------------------

altTitle <- function(variable) {
  # Title, vars_pretty field for variable
  title <- dict_vars %>%
    filter(!is.na(national_dropdown_label),
           var_readable == sym(variable)) %>%
    head(1) %>%
    pull(var_pretty)
  
  title <- str_trim(str_replace_all(title, " (with|without) Disabilities", ""))
  title <- str_trim(str_replace_all(title, " (with|without) Disability", ""))
  
  return(title)
}

# Create non-overlapping quartile buckets
create_non_overlapping_buckets <- function(data) {
  probs <- seq(0, 1, length.out = 5)
  quantiles <- quantile(data, probs, na.rm = TRUE)
  
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
    formatted_ranges <- paste0(head(breaks, -1), "%-", tail(breaks, -1), "%")
  } else {
    # Format with commas for large numbers
    formatted_breaks <- formatC(breaks, format = "f", big.mark = ",", digits = 0)
    formatted_ranges <- paste(head(formatted_breaks, -1), tail(formatted_breaks, -1), sep = "-")
  }
  return(formatted_ranges)
}

render_national_map <- function(selected, palette_selected = "YlOrRd") {
  is_comp <- dict_vars %>%
    filter(var_readable == selected) %>%
    pull(display_type) %>%
    head(1) %>%
    {. == "comp"}
  
  palette <- brewer.pal(4, palette_selected)
  
  if (!is_comp) {
    legend_title <- paste0(dict_vars$var_pretty[which(dict_vars$var_readable == selected)][1])
    
    us_states_with_data <- us_states %>%
      filter(ABBR != "USA") %>%
      select(1:8, estimate = sym(selected)) %>%
      mutate(estimate = as.numeric(gsub(pattern = "[,]", replacement = "", x = estimate)))
    
    buckets <- create_non_overlapping_buckets(us_states_with_data$estimate)
    labels <- format_ranges(buckets, selected)
    
    us_states_with_data <- us_states_with_data %>%
      mutate(estimate_cat = cut(estimate, breaks = buckets, include.lowest = TRUE, labels = labels))
    
    ggplot(data = us_states_with_data) +
      geom_sf(aes(fill = estimate_cat)) +
      scale_fill_manual(values = palette, name = legend_title) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(size = 14),
            legend.box = "horizontal",
            legend.text = element_text(size = 14))
    
  } else {
    base_var <- dict_vars %>%
      filter(var_readable == selected) %>%
      pull(var_base)
    
    comp_var <- dict_vars %>%
      filter(var_base == base_var, var_readable != selected) %>%
      pull(var_readable)
    
    us_states_with_data <- us_states %>%
      filter(ABBR != "USA") %>%
      select(1:8, estimate = sym(selected), estimate_2 = sym(comp_var)) %>%
      mutate(
        estimate = as.numeric(gsub(pattern = "[,]", replacement = "", x = estimate)),
        estimate_2 = as.numeric(gsub(pattern = "[,]", replacement = "", x = estimate_2))
      )
    
    combined_var <- c(us_states_with_data$estimate, us_states_with_data$estimate_2)
    breaks <- create_non_overlapping_buckets(combined_var)
    labels <- format_ranges(breaks, selected)
    
    us_states_with_data <- us_states_with_data %>%
      mutate(
        estimate_cat = cut(estimate, breaks = breaks, include.lowest = TRUE, labels = labels),
        estimate_2_cat = cut(estimate_2, breaks = breaks, include.lowest = TRUE, labels = labels)
      )
    
    shared_scale <- scale_fill_manual(values = palette, drop = FALSE, name = "Legend Title")
    
    map1 <- ggplot(data = us_states_with_data) +
      geom_sf(aes(fill = estimate_cat)) +
      shared_scale +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.box = "horizontal"
        ) +
      ggtitle("People with Disabilities")
    
    map2 <- ggplot(data = us_states_with_data) +
      geom_sf(aes(fill = estimate_2_cat)) +
      shared_scale +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.box = "horizontal"
      ) +
      ggtitle("People without Disabilities")
    
    legend <- cowplot::get_legend(
      map1 +
        theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.box = "horizontal"
        )
    )
    
    combined <- (map1 + map2) / legend + plot_layout(heights = c(10, 3))
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
