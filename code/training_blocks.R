# Training block derivations for A races
# Uses Relative Effort to classify hard efforts by percentile threshold

build_block_data <- function(hard_effort_threshold = NULL, hard_effort_pct = 0.75) {
  # Races + metadata
  source("make_race_identifiers.R")

  # Only A-race blocks (marathons)
  races_blocks <- races_final |>
    dplyr::filter(race_priority == "A", !is.na(block_start_date)) |>
    dplyr::arrange(date)

  # Basic guardrail
  if (nrow(races_blocks) == 0) {
    stop("No A-race blocks found in races_final. Check race_meta and block_length_weeks.")
  }

  # Threshold for hard efforts based on Relative Effort
  if (is.null(hard_effort_threshold)) {
    hard_effort_threshold <- stats::quantile(app_dat$Relative.Effort, hard_effort_pct, na.rm = TRUE)
  }
  if (is.na(hard_effort_threshold)) {
    hard_effort_threshold <- Inf
  }

  activity_runs <- app_dat |>
    dplyr::filter(Activity.Type == "Run") |>
    dplyr::mutate(
      is_hard = Relative.Effort >= hard_effort_threshold
    )

  # Daily effort rollup
  daily_effort <- activity_runs |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      relative_effort = sum(Relative.Effort, na.rm = TRUE),
      avg_relative_effort = mean(Relative.Effort, na.rm = TRUE),
      hard_miles = sum(distance_miles[is_hard], na.rm = TRUE),
      total_run_miles = sum(distance_miles, na.rm = TRUE),
      long_run_miles_max = max(distance_miles, na.rm = TRUE),
      .groups = "drop"
    )

  # Daily block-level dataset
  block_daily <- dplyr::bind_rows(lapply(seq_len(nrow(races_blocks)), function(i) {
    race <- races_blocks[i, ]
    daily_dat |>
      dplyr::filter(date >= race$block_start_date, date <= race$date) |>
      dplyr::mutate(
        race_name = race$name,
        race_date = race$date,
        block_start_date = race$block_start_date,
        block_length_weeks = race$block_length_weeks,
        days_to_race = as.integer(race$date - date)
      )
  })) |>
    dplyr::left_join(daily_effort, by = "date")

  # Weekly block aggregation for aligned plotting
  block_weekly <- block_daily |>
    dplyr::mutate(weeks_to_race = floor(days_to_race / 7)) |>
    dplyr::group_by(race_name, race_date, block_start_date, weeks_to_race) |>
    dplyr::summarise(
      weekly_miles = sum(daily_miles, na.rm = TRUE),
      weekly_relative_effort = sum(relative_effort, na.rm = TRUE),
      avg_readiness = mean(readiness, na.rm = TRUE),
      .groups = "drop"
    )

  # Block summary features (one row per A-race)
  block_summary <- dplyr::bind_rows(lapply(seq_len(nrow(races_blocks)), function(i) {
    race <- races_blocks[i, ]
    block_range <- daily_dat |>
      dplyr::filter(date >= race$block_start_date, date <= race$date)

    activity_block <- activity_runs |>
      dplyr::filter(date >= race$block_start_date, date <= race$date)

    weekly_miles <- block_range |>
      dplyr::group_by(week_monday) |>
      dplyr::summarise(weekly_miles = sum(daily_miles, na.rm = TRUE), .groups = "drop")

    # Tune-up races inside this block (B races)
    tuneups <- races_final |>
      dplyr::filter(
        race_priority == "B",
        date >= race$block_start_date,
        date <= race$date
      )

    total_miles <- sum(block_range$daily_miles, na.rm = TRUE)
    total_run_miles <- sum(activity_block$distance_miles, na.rm = TRUE)
    hard_miles <- sum(activity_block$distance_miles[activity_block$is_hard], na.rm = TRUE)

    dplyr::tibble(
      race_name = race$name,
      race_date = race$date,
      race_type = race$race_type,
      race_distance_miles = race$distance_miles,
      race_time_minutes = race$moving_minutes + (race$moving_seconds / 60),
      block_start_date = race$block_start_date,
      block_length_weeks = race$block_length_weeks,
      total_miles = total_miles,
      avg_weekly_miles = total_miles / race$block_length_weeks,
      peak_week_miles = ifelse(nrow(weekly_miles) == 0, NA, max(weekly_miles$weekly_miles, na.rm = TRUE)),
      long_run_miles_max = ifelse(nrow(activity_block) == 0, NA, max(activity_block$distance_miles, na.rm = TRUE)),
      pct_hard_miles = ifelse(total_run_miles == 0, NA, hard_miles / total_run_miles),
      avg_relative_effort = mean(activity_block$Relative.Effort, na.rm = TRUE),
      avg_acute_load = mean(block_range$acute_load, na.rm = TRUE),
      avg_chronic_load = mean(block_range$chronic_load, na.rm = TRUE),
      avg_readiness = mean(block_range$readiness, na.rm = TRUE),
      ramp_rate_max = max(block_range$ramp_rate, na.rm = TRUE),
      tuneup_race_count = nrow(tuneups),
      hard_effort_threshold = hard_effort_threshold
    )
  }))

  list(
    races_blocks = races_blocks,
    block_daily = block_daily,
    block_weekly = block_weekly,
    block_summary = block_summary
  )
}

# Default for non-reactive use
block_data_default <- build_block_data()
