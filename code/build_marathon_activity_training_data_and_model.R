#!/usr/bin/env Rscript

# Full-history marathon prediction pipeline (single script).
#
# Why this version:
# - Uses all years of run history, not only one marathon block.
# - Combines recent performance signals (5K/10K/HM proxies) with long-term base.
# - Trains on all known races by converting each race result to a
#   marathon-equivalent time via Riegel, then fits a weighted adjustment model.
#
# Outputs:
# 1) Activity-level feature table (one row per run activity).
# 2) Race-level training table used by the model.
# 3) Current marathon prediction as-of latest activity date.
# 4) Model coefficients for inspection.

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(zoo)
})

# ----------------------------- Args / paths -----------------------------------
args <- commandArgs(trailingOnly = TRUE)

input_export_dir <- if (length(args) >= 1) args[[1]] else "data/strava/April_03_2026"
output_activity_csv <- if (length(args) >= 2) args[[2]] else "data/marathon_prediction_activity_features.csv"
output_race_samples_csv <- if (length(args) >= 3) args[[3]] else "data/marathon_prediction_race_samples.csv"
output_prediction_csv <- if (length(args) >= 4) args[[4]] else "data/marathon_prediction_current.csv"
output_coefficients_csv <- if (length(args) >= 5) args[[5]] else "data/marathon_prediction_model_coefficients.csv"

# ----------------------------- Helpers ----------------------------------------
normalize_activity_columns <- function(df) {
  required_cols <- c(
    "Activity.ID",
    "Activity.Date",
    "Activity.Name",
    "Activity.Type",
    "Elapsed.Time",
    "Distance",
    "Moving.Time",
    "Average.Speed",
    "Relative.Effort",
    "Training.Load",
    "Filename"
  )

  for (col_name in required_cols) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <- NA
    }
  }
  df
}

format_minutes_hms <- function(minutes_value) {
  if (is.na(minutes_value)) {
    return(NA_character_)
  }
  total_seconds <- round(minutes_value * 60)
  h <- total_seconds %/% 3600
  m <- (total_seconds %% 3600) %/% 60
  s <- total_seconds %% 60
  sprintf("%d:%02d:%02d", h, m, s)
}

time_to_minutes <- function(x) {
  x_chr <- as.character(x)
  parts <- strsplit(x_chr, ":", fixed = TRUE)
  sapply(parts, function(p) {
    if (length(p) < 2) {
      return(NA_real_)
    }
    if (length(p) == 2) {
      h <- 0
      m <- suppressWarnings(as.numeric(p[1]))
      s <- suppressWarnings(as.numeric(p[2]))
    } else {
      h <- suppressWarnings(as.numeric(p[1]))
      m <- suppressWarnings(as.numeric(p[2]))
      s <- suppressWarnings(as.numeric(p[3]))
    }
    if (any(is.na(c(h, m, s)))) NA_real_ else h * 60 + m + s / 60
  })
}

riegel_to_marathon <- function(time_minutes, distance_miles, marathon_miles = 26.2188, exponent = 1.06) {
  ifelse(
    is.na(time_minutes) | is.na(distance_miles) | distance_miles <= 0,
    NA_real_,
    time_minutes * (marathon_miles / distance_miles)^exponent
  )
}

roll_sum_partial <- function(x, k) {
  zoo::rollapplyr(
    x,
    width = k,
    FUN = function(v) sum(v, na.rm = TRUE),
    partial = TRUE,
    fill = NA_real_
  )
}

roll_min_partial <- function(x, k) {
  zoo::rollapplyr(
    x,
    width = k,
    FUN = function(v) {
      if (all(is.na(v))) {
        NA_real_
      } else {
        min(v, na.rm = TRUE)
      }
    },
    partial = TRUE,
    fill = NA_real_
  )
}

weighted_mean_available <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  if (!any(keep)) {
    return(NA_real_)
  }
  sum(x[keep] * w[keep]) / sum(w[keep])
}

# ----------------------------- Load activities --------------------------------
activities_csv <- file.path(input_export_dir, "activities.csv")
if (!file.exists(activities_csv)) {
  stop("Could not find activities.csv at: ", activities_csv)
}

raw_activities <- read.csv(activities_csv, check.names = TRUE) |>
  normalize_activity_columns()

app_dat <- raw_activities |>
  filter(
    Activity.Type == "Run",
    Elapsed.Time < 100000
  ) |>
  mutate(
    date_time = mdy_hms(Activity.Date),
    date = as.Date(date_time),
    distance_miles = Distance * 0.62137,
    elapsed_minutes = Elapsed.Time / 60,
    moving_hours = Moving.Time %/% 3600,
    moving_minutes = (Moving.Time %% 3600) %/% 60,
    moving_seconds = Moving.Time %% 60,
    moving_total_c = sprintf("%d:%02d:%02d", moving_hours, moving_minutes, moving_seconds),
    avg_speed_mph = if_else(Moving.Time > 0, (distance_miles / Moving.Time) * 3600, NA_real_),
    avg_pace_mile = 60 / avg_speed_mph
  ) |>
  filter(!is.na(date)) |>
  arrange(date_time)

if (nrow(app_dat) == 0) {
  stop("No run activities found after filtering. Cannot continue.")
}

hard_effort_threshold <- stats::quantile(app_dat$Relative.Effort, probs = 0.75, na.rm = TRUE)
if (is.na(hard_effort_threshold)) {
  hard_effort_threshold <- Inf
}

app_dat <- app_dat |>
  mutate(is_hard = Relative.Effort >= hard_effort_threshold)

# ----------------------------- Daily features ---------------------------------
daily_dat <- app_dat |>
  group_by(date) |>
  summarise(
    daily_miles = sum(distance_miles, na.rm = TRUE),
    daily_time = sum(elapsed_minutes, na.rm = TRUE),
    daily_runs = n(),
    daily_rel_effort = sum(Relative.Effort, na.rm = TRUE),
    daily_training_load = sum(Training.Load, na.rm = TRUE),
    daily_hard_miles = sum(distance_miles[is_hard], na.rm = TRUE),
    daily_longest_run = ifelse(all(is.na(distance_miles)), 0, max(distance_miles, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  complete(
    date = seq(min(date), max(date), by = "day"),
    fill = list(
      daily_miles = 0,
      daily_time = 0,
      daily_runs = 0,
      daily_rel_effort = 0,
      daily_training_load = 0,
      daily_hard_miles = 0,
      daily_longest_run = 0
    )
  ) |>
  arrange(date) |>
  mutate(
    lifetime_miles = cumsum(daily_miles),
    days_running = as.numeric(date - min(date)) + 1,
    years_running = pmax(days_running / 365.25, 0.01),
    lifetime_miles_per_year = lifetime_miles / years_running,
    miles_42d = roll_sum_partial(daily_miles, 42),
    miles_84d = roll_sum_partial(daily_miles, 84),
    miles_180d = roll_sum_partial(daily_miles, 180),
    miles_365d = roll_sum_partial(daily_miles, 365),
    miles_730d = roll_sum_partial(daily_miles, 730),
    hard_miles_42d = roll_sum_partial(daily_hard_miles, 42),
    run_days_42d = roll_sum_partial(as.numeric(daily_miles > 0), 42),
    long_runs_16_365d = roll_sum_partial(as.numeric(daily_longest_run >= 16), 365),
    long_runs_20_365d = roll_sum_partial(as.numeric(daily_longest_run >= 20), 365),
    effort_42d = roll_sum_partial(daily_rel_effort, 42),
    effort_180d = roll_sum_partial(daily_rel_effort, 180),
    avg_pace_42d = if_else(miles_42d > 0, roll_sum_partial(daily_time, 42) / miles_42d, NA_real_),
    recent_to_long_load_ratio = if_else(
      miles_365d > 0,
      miles_42d / (miles_365d / (365 / 42)),
      NA_real_
    )
  )

# ------------------- Multi-distance performance features ----------------------
benchmarks <- tibble::tribble(
  ~bench_name, ~bench_miles, ~tolerance_pct, ~window_days, ~blend_weight,
  "5k",        3.10686,      0.20,           120,          0.20,
  "10k",       6.21371,      0.20,           180,          0.35,
  "half",      13.1094,      0.25,           365,          0.45
)

benchmark_daily <- daily_dat |>
  select(date)

for (i in seq_len(nrow(benchmarks))) {
  bench <- benchmarks[i, ]
  bench_name <- bench$bench_name[[1]]
  bench_miles <- bench$bench_miles[[1]]
  tolerance <- bench$tolerance_pct[[1]]
  window_days <- as.integer(bench$window_days[[1]])

  # Approximate best effort for each benchmark from activities near that distance.
  day_best <- app_dat |>
    filter(
      !is.na(avg_pace_mile),
      distance_miles > 0,
      abs(distance_miles - bench_miles) <= bench_miles * tolerance
    ) |>
    mutate(bench_time_minutes = avg_pace_mile * bench_miles) |>
    group_by(date) |>
    summarise(
      day_best_time = min(bench_time_minutes, na.rm = TRUE),
      .groups = "drop"
    )

  benchmark_daily <- benchmark_daily |>
    left_join(day_best, by = "date")

  day_col <- "day_best_time"
  rolling_col <- paste0("best_", bench_name, "_", window_days, "d")
  eq_col <- paste0("eq_mara_from_", bench_name)

  benchmark_daily[[rolling_col]] <- roll_min_partial(benchmark_daily[[day_col]], window_days)
  benchmark_daily[[eq_col]] <- riegel_to_marathon(
    benchmark_daily[[rolling_col]],
    distance_miles = bench_miles
  )

  benchmark_daily[[day_col]] <- NULL
}

daily_features <- daily_dat |>
  left_join(benchmark_daily, by = "date") |>
  rowwise() |>
  mutate(
    recent_perf_eq_minutes = weighted_mean_available(
      c(eq_mara_from_5k, eq_mara_from_10k, eq_mara_from_half),
      c(0.20, 0.35, 0.45)
    ),
    recent_perf_sources = sum(!is.na(c(eq_mara_from_5k, eq_mara_from_10k, eq_mara_from_half)))
  ) |>
  ungroup() |>
  mutate(
    # Fallback when no benchmark-like efforts exist in recent windows.
    recent_perf_eq_minutes_filled = if_else(
      !is.na(recent_perf_eq_minutes),
      recent_perf_eq_minutes,
      avg_pace_42d * 26.2188 * 0.93
    ),
    log_lifetime_miles = log(lifetime_miles + 1)
  )

# One row per activity, with full-history context attached.
activity_features <- app_dat |>
  left_join(daily_features, by = "date") |>
  arrange(date_time)

# ----------------------------- Race training data -----------------------------
race_activity_filenames <- c(
  "activities/12216806825.fit.gz", # Ogden 2024
  "activities/12774395760.fit.gz", # Tracksmith Twilight 5K
  "activities/13176936233.fit.gz", # Charles River Marathon 2024
  "activities/13662590277.fit.gz", # Cambridge Half
  "activities/14976741299.fit.gz", # Cambridge Classic 5K 2025
  "activities/15264580215.fit.gz", # James Joyce 10K 2025
  "activities/15482005847.fit.gz", # Ogden 2025
  "activities/16016511436.fit.gz", # Miller Mile
  "activities/16805210241.fit.gz", # CRMI 2025
  "activities/17491618524.fit.gz"  # Boston Half 2025
)

races <- data.frame(
  race_name = c(
    "Ogden Marathon 2024",
    "Tracksmith Twilight 5000",
    "Charles River Marathon 2024",
    "Cambridge Half Marathon",
    "Cambridge Spring Classic 5K 2025",
    "James Joyce Ramble 10K 2025",
    "Ogden Marathon 2025",
    "Miller Mile",
    "Charles River Marathon Invitational 2025",
    "Boston Half-Marathon 2025"
  ),
  race_activity_filename = race_activity_filenames
) |>
  left_join(
    app_dat |>
      select(
        Filename,
        race_date = date,
        race_distance_miles = distance_miles,
        moving_total_c
      ),
    by = c("race_activity_filename" = "Filename")
  ) |>
  mutate(
    race_time_minutes = time_to_minutes(moving_total_c)
  )

race_meta <- tibble::tribble(
  ~race_name,                                   ~race_priority, ~race_type,
  "Ogden Marathon 2024",                        "A",            "Marathon",
  "Tracksmith Twilight 5000",                   "C",            "5K",
  "Charles River Marathon 2024",                "A",            "Marathon",
  "Cambridge Half Marathon",                    "B",            "Half",
  "Cambridge Spring Classic 5K 2025",           "B",            "5K",
  "James Joyce Ramble 10K 2025",                "B",            "10K",
  "Ogden Marathon 2025",                        "A",            "Marathon",
  "Miller Mile",                                "C",            "Mile",
  "Charles River Marathon Invitational 2025",   "A",            "Marathon",
  "Boston Half-Marathon 2025",                  "B",            "Half"
)

races_final <- races |>
  left_join(race_meta, by = "race_name") |>
  filter(!is.na(race_date), !is.na(race_distance_miles), !is.na(race_time_minutes)) |>
  mutate(
    race_marathon_equiv_minutes = riegel_to_marathon(race_time_minutes, race_distance_miles),
    feature_date = race_date - 1,
    sample_weight = case_when(
      race_distance_miles >= 20 ~ 1.00,
      race_distance_miles >= 12 ~ 0.90,
      race_distance_miles >= 6 ~ 0.75,
      race_distance_miles >= 3 ~ 0.65,
      TRUE ~ 0.55
    )
  )

race_samples <- races_final |>
  left_join(
    daily_features |>
      select(
        date,
        recent_perf_eq_minutes,
        recent_perf_eq_minutes_filled,
        recent_perf_sources,
        lifetime_miles,
        log_lifetime_miles,
        miles_365d,
        miles_730d,
        miles_42d,
        hard_miles_42d,
        long_runs_16_365d,
        long_runs_20_365d,
        run_days_42d,
        recent_to_long_load_ratio
      ),
    by = c("feature_date" = "date")
  ) |>
  mutate(
    delta_target = race_marathon_equiv_minutes - recent_perf_eq_minutes_filled
  )

model_data <- race_samples |>
  filter(
    !is.na(delta_target),
    !is.na(log_lifetime_miles),
    !is.na(miles_365d),
    !is.na(long_runs_16_365d),
    !is.na(recent_to_long_load_ratio),
    !is.na(sample_weight)
  )

if (nrow(model_data) < 5) {
  stop("Not enough race samples with complete features to fit the full-history model.")
}

# ----------------------------- Model fitting ----------------------------------
# Model predicts the adjustment from recent performance to marathon-equivalent.
delta_model <- stats::lm(
  delta_target ~ log_lifetime_miles + miles_365d + long_runs_16_365d + recent_to_long_load_ratio,
  data = model_data,
  weights = sample_weight
)

model_data <- model_data |>
  mutate(
    train_pred_delta_raw = as.numeric(stats::predict(delta_model, newdata = model_data))
  )

# Leave-one-out cross-validation.
loocv_pred_delta_raw <- rep(NA_real_, nrow(model_data))
for (i in seq_len(nrow(model_data))) {
  train <- model_data[-i, , drop = FALSE]
  test <- model_data[i, , drop = FALSE]
  fit_i <- stats::lm(
    delta_target ~ log_lifetime_miles + miles_365d + long_runs_16_365d + recent_to_long_load_ratio,
    data = train,
    weights = sample_weight
  )
  loocv_pred_delta_raw[i] <- as.numeric(stats::predict(fit_i, newdata = test))
}

# Baseline is recent performance only (no long-term adjustment).
baseline_error <- model_data$race_marathon_equiv_minutes - model_data$recent_perf_eq_minutes_filled
baseline_loocv_rmse <- sqrt(mean(baseline_error^2, na.rm = TRUE))

raw_train_pred <- model_data$recent_perf_eq_minutes_filled + model_data$train_pred_delta_raw
raw_loocv_pred <- model_data$recent_perf_eq_minutes_filled + loocv_pred_delta_raw
raw_loocv_rmse <- sqrt(mean((model_data$race_marathon_equiv_minutes - raw_loocv_pred)^2, na.rm = TRUE))

# Stabilize adjustments:
# - Bound to observed historical delta range.
# - Shrink when LOOCV reliability is weak.
delta_lower <- min(model_data$delta_target, na.rm = TRUE)
delta_upper <- max(model_data$delta_target, na.rm = TRUE)
shrink_factor <- max(0.20, min(0.80, baseline_loocv_rmse / (baseline_loocv_rmse + raw_loocv_rmse)))

stabilize_delta <- function(delta_raw) {
  pmin(pmax(delta_raw, delta_lower), delta_upper) * shrink_factor
}

model_data <- model_data |>
  mutate(
    train_pred_delta = stabilize_delta(train_pred_delta_raw),
    train_pred_marathon_equiv = recent_perf_eq_minutes_filled + train_pred_delta,
    train_error_minutes = race_marathon_equiv_minutes - train_pred_marathon_equiv,
    loocv_pred_delta_raw = loocv_pred_delta_raw,
    loocv_pred_delta = stabilize_delta(loocv_pred_delta_raw),
    loocv_pred_marathon_equiv = recent_perf_eq_minutes_filled + loocv_pred_delta,
    loocv_error_minutes = race_marathon_equiv_minutes - loocv_pred_marathon_equiv
  )

train_rmse <- sqrt(mean(model_data$train_error_minutes^2, na.rm = TRUE))
train_mae <- mean(abs(model_data$train_error_minutes), na.rm = TRUE)
loocv_rmse <- sqrt(mean(model_data$loocv_error_minutes^2, na.rm = TRUE))
loocv_mae <- mean(abs(model_data$loocv_error_minutes), na.rm = TRUE)

# ---------------------------- Current prediction ------------------------------
as_of_date <- max(daily_features$date, na.rm = TRUE)
current_snapshot <- daily_features |>
  filter(date == as_of_date)

if (nrow(current_snapshot) != 1) {
  stop("Could not isolate current snapshot row.")
}

# Avoid extrapolating outside the training feature range.
cap_to_training_range <- function(value, train_values) {
  pmin(pmax(value, min(train_values, na.rm = TRUE)), max(train_values, na.rm = TRUE))
}

current_snapshot_model <- current_snapshot |>
  mutate(
    log_lifetime_miles = cap_to_training_range(log_lifetime_miles, model_data$log_lifetime_miles),
    miles_365d = cap_to_training_range(miles_365d, model_data$miles_365d),
    long_runs_16_365d = cap_to_training_range(long_runs_16_365d, model_data$long_runs_16_365d),
    recent_to_long_load_ratio = cap_to_training_range(recent_to_long_load_ratio, model_data$recent_to_long_load_ratio)
  )

current_pred_delta_raw <- as.numeric(stats::predict(delta_model, newdata = current_snapshot_model))
current_pred_delta <- stabilize_delta(current_pred_delta_raw)
current_pred_minutes <- current_snapshot$recent_perf_eq_minutes_filled + current_pred_delta
current_baseline_minutes <- current_snapshot$recent_perf_eq_minutes_filled

current_prediction <- tibble(
  as_of_date = as_of_date,
  predicted_marathon_minutes = current_pred_minutes,
  predicted_marathon_hms = format_minutes_hms(current_pred_minutes),
  baseline_recent_perf_minutes = current_baseline_minutes,
  baseline_recent_perf_hms = format_minutes_hms(current_baseline_minutes),
  loocv_rmse_minutes = loocv_rmse,
  loocv_mae_minutes = loocv_mae,
  prediction_low_minutes = current_pred_minutes - loocv_rmse,
  prediction_high_minutes = current_pred_minutes + loocv_rmse,
  prediction_low_hms = format_minutes_hms(current_pred_minutes - loocv_rmse),
  prediction_high_hms = format_minutes_hms(current_pred_minutes + loocv_rmse),
  total_lifetime_miles = current_snapshot$lifetime_miles,
  miles_365d = current_snapshot$miles_365d,
  miles_42d = current_snapshot$miles_42d,
  long_runs_16_365d = current_snapshot$long_runs_16_365d,
  recent_to_long_load_ratio = current_snapshot$recent_to_long_load_ratio,
  recent_perf_sources = current_snapshot$recent_perf_sources,
  delta_adjustment_raw = current_pred_delta_raw,
  delta_adjustment_applied = current_pred_delta,
  delta_adjustment_shrink_factor = shrink_factor,
  delta_adjustment_lower_bound = delta_lower,
  delta_adjustment_upper_bound = delta_upper,
  baseline_loocv_rmse_minutes = baseline_loocv_rmse,
  raw_model_loocv_rmse_minutes = raw_loocv_rmse
)

coef_table <- as.data.frame(summary(delta_model)$coefficients) |>
  tibble::rownames_to_column("term") |>
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    t_value = `t value`,
    p_value = `Pr(>|t|)`
  )

# ------------------------------- Save outputs ---------------------------------
dir.create(dirname(output_activity_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_race_samples_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_prediction_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_coefficients_csv), recursive = TRUE, showWarnings = FALSE)

write.csv(activity_features, output_activity_csv, row.names = FALSE)
write.csv(model_data, output_race_samples_csv, row.names = FALSE)
write.csv(current_prediction, output_prediction_csv, row.names = FALSE)
write.csv(coef_table, output_coefficients_csv, row.names = FALSE)

message("Built full-history activity features: ", output_activity_csv)
message("Built race training samples: ", output_race_samples_csv)
message("Built current marathon prediction: ", output_prediction_csv)
message("Built model coefficients: ", output_coefficients_csv)
message("Race samples used: ", nrow(model_data))
message("Train RMSE: ", round(train_rmse, 2), " min | Train MAE: ", round(train_mae, 2), " min")
message("LOOCV RMSE: ", round(loocv_rmse, 2), " min | LOOCV MAE: ", round(loocv_mae, 2), " min")
message("Current predicted marathon: ", round(current_pred_minutes, 2), " min (", format_minutes_hms(current_pred_minutes), ")")
