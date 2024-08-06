# Load necessary libraries
library(dplyr)
library(scales)
library(openxlsx)

# Define the function
process_community_participation_data <- function(input_file, output_file, reverse_z_cols_CPR = NULL) {
  # Read the dataset
  national_data <- readRDS(input_file)
  
  # Select 'Community participation' category columns
  community_participation_vars <- c("pwd_commute_public_pct",
                                    "pwd_commute_car_alone_pct",
                                    "pwd_lessthan_highschool_pct",
                                    "pwd_highschoolequiv_pct",
                                    "pwd_degree_aa_pct",
                                    "pwd_degree_grtoeq_ba_pct",
                                    "pwd_19_64_uninsured_pct",
                                    "pwd_grtoeq_65_uninsured_pct",
                                    "pwd_car_commute_pct",
                                    "pwd_pub_transit_pct",
                                    "pwd_walk_bike_pct",
                                    "pwd_int_pct",
                                    "pwd_smartphone_pct",
                                    "pwd_computer_pct"
                                    )
  
  community_participation_data <- national_data %>% select(NAME, all_of(community_participation_vars))
  
  # Calculate z-scores and cap them from -3 to 3
  community_participation_z_scores <- community_participation_data %>% 
    mutate(across(all_of(community_participation_vars), ~ pmin(pmax(scale(.)[, 1], -3), 3), .names = "z_{col}"))
  
  # Reverse the z-scores for specified columns
  if (!is.null(reverse_z_cols_CPR)) {
    reverse_z_cols_CPR <- paste0("z_", reverse_z_cols_CPR)
    community_participation_z_scores <- community_participation_z_scores %>% 
      mutate(across(all_of(reverse_z_cols_CPR), ~ -1 * .))
  }
  
  # Transform z-scores onto a 100-point scale
  community_participation_scores <- community_participation_z_scores %>%
    mutate(across(starts_with("z_"), ~ rescale(., to = c(0, 100)), .names = "score_{col}"))
  
  # Save the transformed data to an Excel file
  write.xlsx(community_participation_scores, output_file)
  
  return(community_participation_scores)
}

# Example usage
input_file <- "data/final/national_data.Rds"
output_file <- "scorecard/scorecard_data/raw/community_participation__relative_scores.xlsx"
reverse_z_cols_CPR <- c("pwd_commute_car_alone_pct",
                        "pwd_lessthan_highschool_pct",
                        "pwd_19_64_uninsured_pct",
                        "pwd_grtoeq_65_uninsured_pct"
                        )
  
result <- process_community_participation_data(input_file, output_file, reverse_z_cols_CPR)

# Display the result
print(result)
