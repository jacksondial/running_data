pacman::p_load(
  dplyr,
  lubridate,
  ggplot2,
  shinydashboard,
  bslib,
  shiny
)
print(getwd())
source("riegel_calculation.R")

app_dat <- readRDS("../data/activity_dat2.RDS")
running_palette <- c(
  "#002147", # Dark Navy Blue
  "#A87C55", # Warm Camel Brown
  "#FFD700", # Gold Yellow
  "#8B1D1D", # Rich Burgundy Red
  "#004E64", # Deep Teal
  "#F4EDE4", # Warm Beige
  "#3E2C1C", # Deep Brown
  "#D4AF37", # Brass Gold
  "#3B5A52", # Forest Green
  "#C65353"  # Coral Red
)
# Function to convert from minutes to a formatted duration
format_duration <- function(minutes) {
  duration <- seconds_to_period(minutes * 60)  # Convert minutes to seconds
  sprintf("%d hours, %d minutes, and %d seconds", 
          as.integer(duration@hour), 
          as.integer(duration@minute), 
          as.integer(duration@.Data))  # Convert to integer explicitly
}
