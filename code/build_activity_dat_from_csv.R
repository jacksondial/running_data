pacman::p_load(
  dplyr,
  lubridate
)

normalize_activity_columns <- function(df) {
  required_cols <- c(
    "Activity.ID",
    "Activity.Date",
    "Activity.Name",
    "Activity.Type",
    "Activity.Description",
    "Elapsed.Time",
    "Distance",
    "Moving.Time",
    "Average.Speed",
    "Max.Heart.Rate",
    "Average.Heart.Rate",
    "Relative.Effort",
    "Training.Load",
    "Intensity",
    "Recovery",
    "Activity.Gear",
    "Start.Time"
  )

  for (col_name in required_cols) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <- NA
    }
  }

  df
}

build_activity_dat_from_csv <- function(
  source_csv = "data/strava/api_export/activities_api_compat.csv",
  output_rds = "data/activity_dat2.RDS"
) {
  activity_dat <- read.csv(source_csv, check.names = TRUE) |>
    normalize_activity_columns() |>
    dplyr::select(where(~ !all(is.na(.)))) |>
    dplyr::filter(
      Activity.Type == "Run",
      Elapsed.Time < 100000
    )

  activity_dat2 <- activity_dat |>
    dplyr::select(where(~ !all(is.na(.)))) |>
    dplyr::mutate(
      date_time = mdy_hms(Activity.Date),
      date = date(mdy_hms(Activity.Date)),
      day = day(date),
      month = month(date),
      year = year(date),
      distance_miles = Distance * 0.62137,
      elapsed_minutes = Elapsed.Time / 60,
      moving_hours = Moving.Time %/% 3600,
      moving_minutes = (Moving.Time %% 3600) %/% 60,
      moving_seconds = Moving.Time %% 60,
      moving_total_c = sprintf("%d:%02d:%02d", moving_hours, moving_minutes, moving_seconds),
      minutes = trunc(elapsed_minutes),
      seconds = Elapsed.Time %% 60,
      week = lubridate::week(date),
      week_monday = isoweek(date),
      year_monday = isoyear(date),
      avg_speed_mph = dplyr::if_else(Moving.Time > 0, (distance_miles / Moving.Time) * 3600, NA_real_),
      avg_pace_mile = 60 / avg_speed_mph,
      avg_pace_mile_min = trunc(avg_pace_mile),
      avg_pace_mile_sec = trunc(avg_pace_mile %% 1 * 60),
      avg_pace_mile_c = dplyr::case_when(
        nchar(avg_pace_mile_sec) == 2 ~ paste0(avg_pace_mile_min, ":", avg_pace_mile_sec),
        nchar(avg_pace_mile_sec) == 1 ~ paste0(avg_pace_mile_min, ":0", avg_pace_mile_sec),
        TRUE ~ NA_character_
      )
    )

  saveRDS(activity_dat2, output_rds)
  invisible(activity_dat2)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  source_csv <- if (length(args) >= 1) args[[1]] else "data/strava/api_export/activities_api_compat.csv"
  output_rds <- if (length(args) >= 2) args[[2]] else "data/activity_dat2.RDS"
  build_activity_dat_from_csv(source_csv = source_csv, output_rds = output_rds)
}
