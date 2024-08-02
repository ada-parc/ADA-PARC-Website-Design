# Load necessary libraries
library(dplyr)
library(scales)
library(openxlsx)

# Define the function
process_community_living_data <- function(input_file, output_file, reverse_z_cols = NULL) {
  # Read the dataset
  national_data <- readRDS(input_file)
  
  # Select 'Community Living' category columns
  community_living_vars <- c("pwd_nursing_18_64_pct", 
                             "pwd_grpquarters_institution_pct", 
                             "pwd_grpquarters_noninstitution_pct",
                             "pop_total_grpquarters",
                             "pwd_pct",
                             "pop_grpquarters",
                             "pwd_grpquarters_pct",
                             "pop_grpquarters_noninstitution",
                             "pop_grpquarters_noninstitution_pwd_pct",
                             "pop_grpquarters_institution",
                             "pop_grpquarters_institution_pwd_pct",
                             "pop_nursing",
                             "pop_grpqrters_18_64",
                             "pwd_grpqrters_18_64",
                             "pop_nursing_18_64",
                             "pwod_nursing_18_64",
                             "pwd_nursing_18_64_pct",
                             "pwod_nursing_18_64_pct",
                             "pop_nursing_18_24",
                             "pop_nursing_25_34",
                             "pop_nursing_35_44",
                             "pop_nursing_45_54",
                             "pop_nursing_55_64",
                             "pop_corrections",
                             "pwod_corrections",
                             "pwd_corrections",
                             "pwd_grpquarters",
                             "pwod_grpquarters",
                             "pwd_total_grpquarters",
                             "pwod_total_grpquarters",
                             "pwd_pct_grpquarters",
                             "pwod_pct_grpquarters",
                             "pop_grpquarters_institution_pwod_pct",
                             "pop_grpquarters_noninstitution_pwod_pct",
                             "grpquarters_pct",
                             "pwd_grpquarters_institution",
                             "pwd_grpquarters_institution_pct",
                             "pwod_grpquarters_institution",
                             "pwod_grpquarters_institution_pct",
                             "pwd_grpquarters_noninstitution",
                             "pwd_grpquarters_noninstitution_pct",
                             "pwod_grpquarters_noninstitution",
                             "pwod_grpquarters_noninstitution_pct",
                             "pwd_home",
                             "pwd_home_pct",
                             "pwod_home",
                             "pwod_home_pct",
                             "pwd_grpqrters_18_64pct",
                             "pwod_grpqrters_18_64_pct",
                             "pwod_grpqrters_18_64",
                             "pwd_nursing_18_64",
                             "pwd_corrections_pct",
                             "pwod_corrections_pct",
                             "pwd_housing_choice_voucher_pct",
                             "pwd_pubhousing_pct"
                             
                             )
  
  community_living_data <- national_data %>% select(NAME, all_of(community_living_vars))
  
  # Calculate z-scores and cap them from -3 to 3
  community_living_z_scores <- community_living_data %>% 
    mutate(across(all_of(community_living_vars), ~ pmin(pmax(scale(.)[, 1], -3), 3), .names = "z_{col}"))
  
  # Reverse the z-scores for specified columns
  if (!is.null(reverse_z_cols)) {
    reverse_z_cols <- paste0("z_", reverse_z_cols)
    community_living_z_scores <- community_living_z_scores %>% 
      mutate(across(all_of(reverse_z_cols), ~ -1 * .))
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
output_file <- "data/final/community_living_scores.xlsx"
reverse_z_cols <- c("pwd_grpquarters_pct", "pop_grpquarters_noninstitution_pwd_pct", "pop_grpquarters_institution_pwd_pct", "")
result <- process_community_living_data(input_file, output_file, reverse_z_cols)

# Display the result
print(result)

