# Makes landing page objects
source("init.R")
current_year <- lubridate::year(Sys.Date())
last_year <- current_year - 1

run_activities <- app_dat |>
  dplyr::filter(Activity.Type == "Run")

year_run_data <- function(year_value) {
  run_activities |>
    dplyr::filter(year == as.character(year_value))
}

year_miles <- function(year_value) {
  year_run_data(year_value) |>
    dplyr::summarise(value = sum(distance_miles, na.rm = TRUE)) |>
    dplyr::pull(value) |>
    round(2)
}

fmt_stat <- function(x, digits = 1) {
  format(round(x, digits), nsmall = digits, big.mark = ",", scientific = FALSE, trim = TRUE)
}

latest <- daily_dat |>
  dplyr::filter(date == max(date))

this_year_weekly_avg <- year_run_data(current_year) |>
  dplyr::group_by(week_monday) |>
  dplyr::summarise(weekly_miles = sum(distance_miles, na.rm = TRUE), .groups = "drop") |>
  dplyr::summarise(value = mean(weekly_miles, na.rm = TRUE)) |>
  dplyr::pull(value)
this_year_weekly_avg <- ifelse(is.na(this_year_weekly_avg), 0, this_year_weekly_avg)

this_year_longest <- year_run_data(current_year) |>
  dplyr::summarise(value = max(distance_miles, na.rm = TRUE)) |>
  dplyr::pull(value)
this_year_longest <- ifelse(is.infinite(this_year_longest), 0, this_year_longest)

lifetime_miles_vb <- bslib::value_box(
  title = "Lifetime Miles",
  value = fmt_stat(sum(run_activities$distance_miles, na.rm = TRUE)),
  showcase = icon("road"),
  theme = value_box_theme(bg = "#1B2028", fg = "#4FD1C5"),
  class = "lp-stat-card",
  fill = TRUE,
  height = 175L
)

miles_this_year_vb <- bslib::value_box(
  title = paste0(current_year, " Miles"),
  value = fmt_stat(year_miles(current_year)),
  showcase = icon("person-running"),
  theme = value_box_theme(bg = "#1B2028", fg = "#F9C846"),
  class = "lp-stat-card",
  fill = TRUE,
  height = 175L
)

miles_last_year_vb <- bslib::value_box(
  title = paste0(last_year, " Miles"),
  value = fmt_stat(year_miles(last_year)),
  showcase = icon("calendar-days"),
  theme = value_box_theme(bg = "#1B2028", fg = "#5FA8D3"),
  class = "lp-stat-card",
  fill = TRUE,
  height = 175L
)

avg_weekly_this_year_vb <- bslib::value_box(
  title = paste0(current_year, " Avg Weekly Miles"),
  value = fmt_stat(this_year_weekly_avg),
  showcase = icon("chart-line"),
  theme = value_box_theme(bg = "#1B2028", fg = "#F97316"),
  class = "lp-stat-card",
  fill = TRUE,
  height = 175L
)

longest_run_this_year_vb <- bslib::value_box(
  title = paste0("Longest Run in ", current_year),
  value = paste0(fmt_stat(this_year_longest), " mi"),
  showcase = icon("mountain"),
  theme = value_box_theme(bg = "#1B2028", fg = "#C084FC"),
  class = "lp-stat-card",
  fill = TRUE,
  height = 175L
)

readiness_box <- bslib::value_box(
  title = "Current Readiness Signal",
  value = latest$insight,
  showcase = icon("heartbeat"),
  theme = value_box_theme(bg = "#1B2028", fg = "#F43F5E"),
  class = "lp-stat-card lp-stat-card--readiness",
  fill = TRUE,
  height = 175L
)
