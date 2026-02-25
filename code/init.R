pacman::p_load(
  dplyr,
  lubridate,
  ggplot2,
  shinydashboard,
  bslib,
  shiny,
  tidyr,
  glue,
  ggiraph
)
print(getwd())
source("riegel_calculation.R")

app_dat <- readRDS("../data/activity_dat2.RDS")
running_palette <- c(
  "#0B2545",  # Deep performance navy
  "#1B998B",  # Fresh teal
  "#E84855",  # Energetic red
  "#F9C846",  # Warm gold
  "#2E7D32",  # Strong green
  "#5FA8D3",  # Sky blue
  "#6C4AB6",  # Modern purple accent
  "#F4F1EC",  # Clean warm light
  "#2F2F2F",  # Soft black / charcoal
  "#FF9F1C"   # Bright orange accent
)

# Function to convert from minutes to a formatted duration
format_duration <- function(minutes) {
  duration <- seconds_to_period(minutes * 60)  # Convert minutes to seconds
  sprintf("%d hours, %d minutes, and %d seconds", 
          as.integer(duration@hour), 
          as.integer(duration@minute), 
          as.integer(duration@.Data))  # Convert to integer explicitly
}

source("data_derivations.R")
source("training_blocks.R")
