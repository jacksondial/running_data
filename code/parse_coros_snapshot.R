# Parse the COROS snapshot extracts into tidy CSVs the app reads.
#
# Run from the repo root (or anywhere -- paths are resolved with here::here):
#   Rscript code/parse_coros_snapshot.R
#
# Inputs : data/coros/snapshot/*.txt   (see that folder's README for provenance)
# Outputs: data/coros/coros_daily.csv      one row per calendar day
#          data/coros/coros_activities.csv one row per run
#          data/coros/coros_summary.csv    single-row scalars (fitness + recovery)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

snap_dir <- here::here("data", "coros", "snapshot")
out_dir <- here::here("data", "coros")

# Everything is read as character on purpose: readr guesses "48:33" as an hms of
# 48 HOURS 33 minutes. Columns are converted explicitly below instead.
read_psv <- function(name) {
  readr::read_delim(
    file.path(snap_dir, name),
    delim = "|",
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

# "1:05:09" -> 65.15 minutes; "48:33" -> 48.55 minutes
hms_to_minutes <- function(x) {
  vapply(strsplit(x, ":", fixed = TRUE), function(parts) {
    p <- as.numeric(parts)
    if (length(p) == 3) p[1] * 60 + p[2] + p[3] / 60
    else if (length(p) == 2) p[1] + p[2] / 60
    else NA_real_
  }, numeric(1))
}

# "7h 35min" -> 455 minutes
hm_to_minutes <- function(x) {
  h <- as.numeric(sub("^\\s*(\\d+)h.*$", "\\1", x))
  m <- as.numeric(sub("^.*?(\\d+)min\\s*$", "\\1", x))
  h * 60 + m
}

# Pull "Label: value" out of a verbatim tool-output block.
scalar_field <- function(lines, label) {
  hit <- grep(paste0("^", label, ":"), lines, value = TRUE)
  if (length(hit) == 0) return(NA_character_)
  trimws(sub(paste0("^", label, ":"), "", hit[1]))
}

# ---- activities ------------------------------------------------------------
activities <- read_psv("activities.txt") |>
  mutate(
    date = as.Date(date),
    across(c(distance_km, avg_hr, calories), as.numeric),
    duration_min = hms_to_minutes(duration),
    pace_min_km = hms_to_minutes(avg_pace_km),
    distance_mi = distance_km * 0.621371,
    pace_min_mi = pace_min_km / 0.621371,
    is_workout = sport == "Track Run" | pace_min_km <= 4.6
  ) |>
  arrange(date)

# ---- daily streams ---------------------------------------------------------
load_dat <- read_psv("training_load.txt") |>
  mutate(
    date = as.Date(date),
    across(c(short_term_load, long_term_load, load_ratio), as.numeric)
  ) |>
  rename(load_comment = comment)

stress_dat <- read_psv("stress.txt") |>
  mutate(date = as.Date(date), avg_stress = as.numeric(avg_stress)) |>
  rename(stress_label = label)

sleep_dat <- read_psv("sleep.txt") |>
  mutate(
    date = as.Date(date),
    across(c(sleep_score, deep_pct, light_pct, rem_pct, awake_pct,
             awake_min, awake_count), as.numeric),
    sleep_min = hm_to_minutes(main_sleep),
    sleep_hours = sleep_min / 60
  ) |>
  select(-main_sleep)

rhr_lines <- readLines(file.path(snap_dir, "resting_hr.txt"), warn = FALSE)
rhr_dat <- tibble(line = grep("^\\d{4}-\\d{2}-\\d{2}:", rhr_lines, value = TRUE)) |>
  separate(line, into = c("date", "value"), sep = ":\\s*", extra = "merge") |>
  mutate(
    date = as.Date(date),
    # "No data" days stay NA rather than being dropped or carried forward
    resting_hr = suppressWarnings(as.numeric(sub(" bpm", "", value)))
  ) |>
  select(date, resting_hr)

hrv_lines <- readLines(file.path(snap_dir, "hrv.txt"), warn = FALSE)
hrv_dat <- tibble(line = grep("^\\d{4}-\\d{2}-\\d{2}:", hrv_lines, value = TRUE)) |>
  mutate(
    date = as.Date(sub(":.*$", "", line)),
    hrv_ms = as.numeric(sub("^.*HRV Avg:\\s*(\\d+) ms.*$", "\\1", line)),
    hrv_status = trimws(sub("^.*ms\\s*—\\s*([^|]+)\\|.*$", "\\1", line)),
    hrv_low = as.numeric(sub("^.*Normal Range:\\s*(\\d+)\\s*-.*$", "\\1", line)),
    hrv_high = as.numeric(sub("^.*Normal Range:\\s*\\d+\\s*-\\s*(\\d+) ms.*$", "\\1", line)),
    hrv_baseline = as.numeric(sub("^.*Baseline:\\s*(\\d+) ms.*$", "\\1", line))
  ) |>
  select(-line)

# Per-day running totals, so load/recovery can be read against what was actually run
daily_runs <- activities |>
  group_by(date) |>
  summarise(
    run_km = sum(distance_km, na.rm = TRUE),
    run_mi = sum(distance_mi, na.rm = TRUE),
    run_min = sum(duration_min, na.rm = TRUE),
    run_count = n(),
    day_avg_hr = round(weighted.mean(avg_hr, duration_min, na.rm = TRUE)),
    .groups = "drop"
  )

# One row per day across the full span any stream covers; missing stays missing.
all_dates <- Reduce(
  union,
  list(load_dat$date, stress_dat$date, sleep_dat$date,
       rhr_dat$date, hrv_dat$date, daily_runs$date)
) |> as.Date(origin = "1970-01-01")

daily <- tibble(date = seq(min(all_dates), max(all_dates), by = "day")) |>
  left_join(load_dat, by = "date") |>
  left_join(stress_dat, by = "date") |>
  left_join(sleep_dat, by = "date") |>
  left_join(rhr_dat, by = "date") |>
  left_join(hrv_dat, by = "date") |>
  left_join(daily_runs, by = "date") |>
  mutate(
    across(c(run_km, run_mi, run_min, run_count), \(x) tidyr::replace_na(x, 0)),
    week_monday = as.Date(cut(date, "week", start.on.monday = TRUE))
  )

# ---- scalars ---------------------------------------------------------------
fit_lines <- readLines(file.path(snap_dir, "fitness_assessment.txt"), warn = FALSE)
rec_lines <- readLines(file.path(snap_dir, "recovery_status.txt"), warn = FALSE)

summary_row <- tibble(
  snapshot_date = max(daily$date),
  vo2max = as.numeric(scalar_field(fit_lines, "VO2max")),
  running_level = as.numeric(scalar_field(fit_lines, "Running Level")),
  threshold_pace_km = scalar_field(fit_lines, "Threshold Pace"),
  pred_5k = scalar_field(fit_lines, "5 km Prediction"),
  pred_10k = scalar_field(fit_lines, "10 km Prediction"),
  pred_half = scalar_field(fit_lines, "Half Marathon Prediction"),
  pred_marathon = scalar_field(fit_lines, "Marathon Prediction"),
  recovery_pct = as.numeric(sub("%", "", scalar_field(rec_lines, "Recovery"))),
  recovery_level = scalar_field(rec_lines, "Level"),
  full_recovery_in = scalar_field(rec_lines, "Estimated Full Recovery")
)

# ---- write -----------------------------------------------------------------
readr::write_csv(daily, file.path(out_dir, "coros_daily.csv"))
readr::write_csv(activities, file.path(out_dir, "coros_activities.csv"))
readr::write_csv(summary_row, file.path(out_dir, "coros_summary.csv"))

cat("coros_daily.csv     ", nrow(daily), "days   ", format(min(daily$date)), "->", format(max(daily$date)), "\n")
cat("coros_activities.csv", nrow(activities), "runs\n")
cat("coros_summary.csv   ", nrow(summary_row), "row\n")
