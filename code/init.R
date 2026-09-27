# Shared setup: packages and the ggplot theme every chart uses.

pacman::p_load(
  bslib,
  dplyr,
  ggiraph,
  ggplot2,
  glue,
  readr,
  shiny,
  tidyr
)

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

# ---- COROS data, charts, inputs and tabs ----------------------------------

# The tidy CSVs are generated from the tracked snapshot, not tracked themselves;
# build them on first run so a fresh clone works.
if (!file.exists("../data/coros/coros_daily.csv")) source("parse_coros_snapshot.R")

source("coros_data.R")    # COROS snapshot + derived metrics
source("coros_plots.R")   # charts
source("inputs.R")        # input modules
source("ui_styles.R")     # app-wide CSS
source("ui_coros.R")      # tabs
