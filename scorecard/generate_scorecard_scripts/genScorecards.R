# Main R script for scorecards


# Function to source other scripts
source_scripts <- function(script_list) {
  for (script in script_list) {
    tryCatch({
      source(script)
      message(paste("Successfully sourced:", script))
    }, error = function(e) {
      message(paste("Error sourcing:", script, "\n", e))
    })
  }
}

# List of scripts to source
scripts_to_source <- c(
  "scorecard/generate_scorecard_scripts/genCommunity_Living_relative.R",
  "scorecard/generate_scorecard_scripts/genCommunity_Participation_relative.R",
  "scorecard/generate_scorecard_scripts/genWork_Economic_relative.R"

)

# Source the scripts
source_scripts(scripts_to_source)
