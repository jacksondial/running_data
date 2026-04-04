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

theme_running_dark <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#1E242C", color = NA),
      panel.background = ggplot2::element_rect(fill = "#1E242C", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#3A434F", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "#C8D3DF"),
      axis.title = ggplot2::element_text(color = "#E6EEF5"),
      plot.title = ggplot2::element_text(color = "#E6EEF5", face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "#A9B7C6"),
      legend.background = ggplot2::element_rect(fill = "#1E242C", color = NA),
      legend.key = ggplot2::element_rect(fill = "#1E242C", color = NA),
      legend.title = ggplot2::element_text(color = "#E6EEF5"),
      legend.text = ggplot2::element_text(color = "#C8D3DF")
    )
}

palette_categorical <- function(n) {
  if (n <= 0) {
    return(character(0))
  }
  grDevices::hcl.colors(n, palette = "viridis")
}
