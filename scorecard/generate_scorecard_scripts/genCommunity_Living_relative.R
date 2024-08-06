# Load necessary libraries
library(dplyr)
library(scales)
library(openxlsx)

# Define the function
process_community_living_data <- function(input_file, output_file, reverse_z_cols_CLR = NULL) {
  # Read the dataset
  national_data <- readRDS(input_file)
  
  # Select 'Community Living' category columns
  community_living_vars <- c("pwd_nursing_18_64_pct", 
                             "pwd_grpquarters_institution_pct", 
                             "pwd_grpquarters_noninstitution_pct",
                             "pwd_home_pct",
                             "pwd_corrections_pct",
                             "pwd_housing_choice_voucher_pct",
                             "pwd_pubhousing_pct"
                             )
  
  community_living_data <- national_data %>% select(NAME, all_of(community_living_vars))
  
  # Calculate z-scores and cap them from -3 to 3
  community_living_z_scores <- community_living_data %>% 
    mutate(across(all_of(community_living_vars), ~ pmin(pmax(scale(.)[, 1], -3), 3), .names = "z_{col}"))
  
  # Reverse the z-scores for specified columns
  if (!is.null(reverse_z_cols_CLR)) {
    reverse_z_cols_CLR <- paste0("z_", reverse_z_cols_CLR)
    community_living_z_scores <- community_living_z_scores %>% 
      mutate(across(all_of(reverse_z_cols_CLR), ~ -1 * .))
  }
  
  # Transform z-scores onto a 100-point scale
  community_living_scores <- community_living_z_scores %>%
    mutate(across(starts_with("z_"), ~ rescale(., to = c(0, 100)), .names = "score_{col}"))
  
  # Save the transformed data to an Excel file
  write.xlsx(community_living_scores, output_file)
  
  return(community_living_scores)
}

# Example usage
input_file <- "data/final/national_data.Rds"
output_file <- "scorecard/scorecard_data/raw/community_living__relative_scores.xlsx"
reverse_z_cols_CLR <- c("pwd_nursing_18_64_pct", 
                        "pwd_grpquarters_institution_pct", 
                        "pwd_grpquarters_noninstitution_pct",
                        "pwd_corrections_pct",
                        "pwd_housing_choice_voucher_pct",
                        "pwd_pubhousing_pct"
                        )

result <- process_community_living_data(input_file, output_file, reverse_z_cols_CLR)

# Display the result
print(result)

