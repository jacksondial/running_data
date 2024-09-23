pacman::p_load(
  dplyr,
  lubridate,
  ggplot2,
  shinydashboard
)
print(getwd())
app_dat <- readRDS("../data/activity_dat2.RDS")