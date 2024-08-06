# Load necessary libraries
library(dplyr)
library(scales)
library(openxlsx)

# Define the function
process_work_economic_data <- function(input_file, output_file, reverse_z_cols_WER = NULL) {
  # Read the dataset
  national_data <- readRDS(input_file)
  
  # Select 'Community participation' category columns
  work_economic_vars <- c("pwd_below_poverty_pct",
                          "pwd_employed_pct",
                          "pwd_unemployed_pct",
                          "pwd_notlabor_pct",
                          "pwd_grtoeq_16_med_individual_income"
                          )
  
  work_economic_data <- national_data %>% select(NAME, all_of(work_economic_vars))
  
  # Ignore NA / non-numeric
  work_economic_data <- work_economic_data %>%
    mutate(across(all_of(work_economic_vars), ~ as.numeric(.))) %>%
    mutate(across(all_of(work_economic_vars), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  # Calculate z-scores and cap them from -3 to 3
  work_economic_z_scores <- work_economic_data %>% 
    mutate(across(all_of(work_economic_vars), ~ pmin(pmax(scale(.)[, 1], -3), 3), .names = "z_{col}"))
  
  # Reverse the z-scores for specified columns
  if (!is.null(reverse_z_cols_WER)) {
    reverse_z_cols_WER <- paste0("z_", reverse_z_cols_WER)
    work_economic_z_scores <- work_economic_z_scores %>% 
      mutate(across(all_of(reverse_z_cols_WER), ~ -1 * .))
  }
  
  # Transform z-scores onto a 100-point scale
  work_economic_scores <- work_economic_z_scores %>%
    mutate(across(starts_with("z_"), ~ rescale(., to = c(0, 100)), .names = "score_{col}"))
  
  # Save the transformed data to an Excel file
  write.xlsx(work_economic_scores, output_file)
  
  return(work_economic_scores)
}

# Example usage
input_file <- "data/final/national_data.Rds"
output_file <- "scorecard/scorecard_data/raw/work_economic__relative_scores.xlsx"
reverse_z_cols_WER <- c("pwd_below_poverty_pct", "pwd_unemployed_pct", "pwd_notlabor_pct")
result <- process_work_economic_data(input_file, output_file, reverse_z_cols_WER)

# Display the result
print(result)