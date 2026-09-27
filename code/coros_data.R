# COROS data layer.
#
# Reads the tidy CSVs produced by code/parse_coros_snapshot.R and derives the
# taper/race-readiness metrics the COROS panels display. No modelling here --
# everything is either a COROS-reported value or a simple summary of one.

coros_daily <- readr::read_csv(
  "../data/coros/coros_daily.csv",
  col_types = readr::cols(
    date = readr::col_date(),
    week_monday = readr::col_date(),
    load_comment = readr::col_character(),
    stress_label = readr::col_character(),
    hrv_status = readr::col_character(),
    .default = readr::col_double()
  )
)

coros_activities <- readr::read_csv(
  "../data/coros/coros_activities.csv",
  col_types = readr::cols(
    date = readr::col_date(),
    sport = readr::col_character(),
    location = readr::col_character(),
    sport_group = readr::col_character(),
    duration = readr::col_character(),
    avg_pace_km = readr::col_character(),
    is_workout = readr::col_logical(),
    is_marathon_stimulus = readr::col_logical(),
    is_threshold_stimulus = readr::col_logical(),
    .default = readr::col_double()
  )
)

# Prediction/pace fields are strings like "2:43:06" -- read as character so readr
# does not reinterpret them as hms (it reads "17:25" as 17 hours).
coros_summary <- readr::read_csv(
  "../data/coros/coros_summary.csv",
  col_types = readr::cols(
    snapshot_date = readr::col_date(),
    vo2max = readr::col_double(),
    running_level = readr::col_double(),
    recovery_pct = readr::col_double(),
    .default = readr::col_character()
  )
)

coros_snapshot_date <- coros_summary$snapshot_date[1]

# ---- helpers ---------------------------------------------------------------

# 5.183 -> "5:11"
fmt_pace <- function(min_per_unit) {
  if (is.na(min_per_unit)) return("--")
  mins <- floor(min_per_unit)
  secs <- round((min_per_unit - mins) * 60)
  if (secs == 60) { mins <- mins + 1; secs <- 0 }
  sprintf("%d:%02d", mins, secs)
}

fmt_signed <- function(x, digits = 1, unit = "") {
  if (is.na(x)) return("--")
  paste0(if (x > 0) "+" else "", format(round(x, digits), nsmall = digits), unit)
}

# Most recent non-missing value of a column, with its date.
latest_value <- function(df, col) {
  ok <- df[!is.na(df[[col]]), c("date", col)]
  if (nrow(ok) == 0) return(list(value = NA_real_, date = as.Date(NA)))
  ok <- ok[order(ok$date), ]
  list(value = ok[[col]][nrow(ok)], date = ok$date[nrow(ok)])
}

# ---- current state ---------------------------------------------------------

coros_current <- local({
  hrv <- latest_value(coros_daily, "hrv_ms")
  rhr <- latest_value(coros_daily, "resting_hr")
  load_ratio <- latest_value(coros_daily, "load_ratio")
  st_load <- latest_value(coros_daily, "short_term_load")
  lt_load <- latest_value(coros_daily, "long_term_load")

  recent <- coros_daily |> dplyr::filter(date > max(date) - 7)
  prior <- coros_daily |> dplyr::filter(date <= max(date) - 7, date > max(date) - 28)
  prior7 <- coros_daily |> dplyr::filter(date <= max(date) - 7, date > max(date) - 14)

  # unname/as.vector strips the na.action attribute na.omit leaves behind
  last_ok <- function(x) as.vector(dplyr::last(stats::na.omit(x)))

  list(
    hrv = hrv$value,
    hrv_date = hrv$date,
    hrv_baseline = last_ok(coros_daily$hrv_baseline),
    hrv_low = last_ok(coros_daily$hrv_low),
    hrv_high = last_ok(coros_daily$hrv_high),
    hrv_status = last_ok(coros_daily$hrv_status),
    resting_hr = rhr$value,
    rhr_baseline = mean(prior$resting_hr, na.rm = TRUE),
    load_ratio = load_ratio$value,
    short_term_load = st_load$value,
    long_term_load = lt_load$value,
    load_comment = last_ok(coros_daily$load_comment),
    sleep_score_7d = mean(recent$sleep_score, na.rm = TRUE),
    sleep_hours_7d = mean(recent$sleep_hours, na.rm = TRUE),
    stress_7d = mean(recent$avg_stress, na.rm = TRUE),
    miles_7d = sum(recent$run_mi, na.rm = TRUE),
    bike_miles_7d = sum(recent$bike_mi, na.rm = TRUE),
    # Total training TIME is the cross-sport load measure: a 200 mi ride and a
    # 20 mi run are not comparable by distance, but both cost hours and both
    # feed COROS's load/recovery numbers.
    hours_7d = sum(recent$total_min, na.rm = TRUE) / 60,
    run_hours_7d = sum(recent$run_min, na.rm = TRUE) / 60,
    bike_hours_7d = sum(recent$bike_min, na.rm = TRUE) / 60,
    strength_hours_7d = sum(recent$strength_min, na.rm = TRUE) / 60,
    miles_prior_7d = sum(prior7$run_mi, na.rm = TRUE),
    hours_prior_7d = sum(prior7$total_min, na.rm = TRUE) / 60
  )
})

# ---- weekly running volume -------------------------------------------------

# Weekly totals split by sport. Hours is the headline because distance does not
# mean the same thing across running and cycling.
coros_weekly_sport <- coros_activities |>
  dplyr::mutate(week_monday = as.Date(cut(date, "week", start.on.monday = TRUE))) |>
  dplyr::group_by(week_monday, sport_group) |>
  dplyr::summarise(
    hours = sum(duration_min, na.rm = TRUE) / 60,
    miles = sum(distance_mi, na.rm = TRUE),
    sessions = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(week_monday)

coros_weekly <- coros_activities |>
  dplyr::mutate(week_monday = as.Date(cut(date, "week", start.on.monday = TRUE))) |>
  dplyr::group_by(week_monday) |>
  dplyr::summarise(
    hours = sum(duration_min, na.rm = TRUE) / 60,
    run_miles = sum(distance_mi[sport_group == "Run"], na.rm = TRUE),
    bike_miles = sum(distance_mi[sport_group == "Bike"], na.rm = TRUE),
    run_hours = sum(duration_min[sport_group == "Run"], na.rm = TRUE) / 60,
    bike_hours = sum(duration_min[sport_group == "Bike"], na.rm = TRUE) / 60,
    strength_hours = sum(duration_min[sport_group == "Strength"], na.rm = TRUE) / 60,
    sessions = dplyr::n(),
    longest_run_mi = suppressWarnings(max(distance_mi[sport_group == "Run"], na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(longest_run_mi = dplyr::if_else(is.infinite(longest_run_mi), 0, longest_run_mi)) |>
  dplyr::arrange(week_monday)

# ---- readiness signals -----------------------------------------------------
# Each signal returns a status (good / watch / flag) plus a plain-language read.
# Thresholds follow COROS's own framing where it gives one (its load-ratio
# comment, its HRV normal range) and are otherwise conventional taper heuristics.

coros_readiness_signals <- function() {
  cur <- coros_current
  rows <- list()

  add <- function(metric, value, status, note) {
    rows[[length(rows) + 1]] <<- data.frame(
      metric = metric, value = value, status = status, note = note,
      stringsAsFactors = FALSE
    )
  }

  # HRV vs COROS's own normal range
  hrv_delta <- cur$hrv - cur$hrv_baseline
  add(
    "Sleep HRV",
    paste0(round(cur$hrv), " ms"),
    if (isTRUE(cur$hrv >= cur$hrv_low)) "good" else "watch",
    if (isTRUE(cur$hrv >= cur$hrv_low)) {
      paste0("Inside COROS's normal band (", cur$hrv_low, "-", cur$hrv_high,
             " ms). ", fmt_signed(hrv_delta, 0, " ms"), " vs baseline.")
    } else {
      paste0("Below the normal band (", cur$hrv_low, "-", cur$hrv_high,
             " ms). One low night is noise; three in a row is a signal.")
    }
  )

  # Resting HR vs its own recent baseline
  rhr_delta <- cur$resting_hr - cur$rhr_baseline
  add(
    "Resting HR",
    paste0(round(cur$resting_hr), " bpm"),
    if (isTRUE(rhr_delta <= 3)) "good" else "watch",
    paste0(fmt_signed(rhr_delta, 1, " bpm"), " vs the prior 3-week average of ",
           round(cur$rhr_baseline, 1), " bpm.",
           if (isTRUE(rhr_delta > 3)) " A sustained rise often precedes illness or overreaching." else "")
  )

  # Load ratio -- COROS labels this itself
  add(
    "Load Ratio",
    sprintf("%.2f", cur$load_ratio),
    if (identical(cur$load_comment, "Excessive")) "flag"
    else if (isTRUE(cur$load_ratio > 1.3)) "watch" else "good",
    paste0("COROS calls this '", cur$load_comment,
           "'. Short-term ", round(cur$short_term_load),
           " vs long-term ", round(cur$long_term_load), ".")
  )

  # Recovery
  add(
    "Recovery",
    paste0(round(coros_summary$recovery_pct[1]), "%"),
    if (coros_summary$recovery_pct[1] >= 80) "good"
    else if (coros_summary$recovery_pct[1] >= 60) "watch" else "flag",
    coros_summary$recovery_level[1]
  )

  # Sleep
  add(
    "Sleep (7d avg)",
    paste0(round(cur$sleep_score_7d), " score"),
    if (isTRUE(cur$sleep_score_7d >= 80)) "good"
    else if (isTRUE(cur$sleep_score_7d >= 70)) "watch" else "flag",
    paste0(sprintf("%.1f", cur$sleep_hours_7d), " h per night on average. ",
           "Sleep in the last two weeks matters more than race-eve sleep.")
  )

  # Total training load across every sport. Running miles alone understate the
  # week badly when there is a long ride in it.
  hrs_change <- (cur$hours_7d - cur$hours_prior_7d) / cur$hours_prior_7d * 100
  add(
    "Training Time (all sports)",
    paste0(sprintf("%.1f", cur$hours_7d), " h"),
    if (isTRUE(hrs_change <= 10)) "good" else "watch",
    paste0(
      sprintf("%.1f", cur$run_hours_7d), " h run + ",
      sprintf("%.1f", cur$bike_hours_7d), " h bike + ",
      sprintf("%.1f", cur$strength_hours_7d), " h strength. ",
      fmt_signed(hrs_change, 0, "%"), " vs the week before (",
      sprintf("%.1f", cur$hours_prior_7d), " h). ",
      "Cycling does not build run-specific durability, but it still costs recovery."
    )
  )

  # Running volume on its own -- the marathon-specific part of the picture.
  vol_change <- (cur$miles_7d - cur$miles_prior_7d) / cur$miles_prior_7d * 100
  add(
    "Running Volume",
    paste0(round(cur$miles_7d, 1), " mi"),
    if (isTRUE(vol_change <= 5)) "good" else "watch",
    paste0(fmt_signed(vol_change, 0, "%"), " vs the week before (",
           round(cur$miles_prior_7d, 1), " mi). Plus ",
           round(cur$bike_miles_7d, 1), " mi cycling.")
  )

  do.call(rbind, rows)
}

# COROS race predictions as a tidy table
coros_predictions <- function() {
  data.frame(
    Distance = c("5K", "10K", "Half Marathon", "Marathon"),
    `COROS Prediction` = c(
      coros_summary$pred_5k[1], coros_summary$pred_10k[1],
      coros_summary$pred_half[1], coros_summary$pred_marathon[1]
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


# ---- COROS race predictor -------------------------------------------------
# COROS documents its own method rather than publishing a formula. What it says:
#
#  * Running Fitness is an absolute 40-100 running-ability score, and the Race
#    Predictor widget "uses your current fitness calculations" to estimate 5K,
#    10K, half and full marathon times.
#  * The algorithm "calculates these abilities based on your last six weeks of
#    training data", with anything older dropping out of the window.
#  * It is distance-specific rather than one number scaled by a Riegel-style
#    exponent: long runs beyond 30 km move the marathon estimate, while a
#    60-minute threshold run moves the 10K and half estimates.
#  * Running Fitness breaks into four abilities, each with its own criterion:
#    Base (>30 km, aerobic power), Endurance (~10 km, threshold pace),
#    Speed (~3 km, VO2max) and Sprint (~400 m, best 30-60 s pace).
#  * Pace zones come from a Pace-Duration Model fitting Maximal Speed,
#    Anaerobic Work and Critical Speed (close to threshold pace).
#  * Predictions assume "ideal weather and course conditions".
#
# Sources are listed in the Race Predictor tab. Nothing below re-implements the
# prediction -- it surfaces COROS's own numbers and shows which sessions inside
# the six-week window are the ones COROS says drive them.

COROS_WINDOW_DAYS <- 42  # COROS's stated six-week rolling window

coros_predictor_inputs <- function() {
  window_start <- coros_snapshot_date - COROS_WINDOW_DAYS

  in_window <- coros_activities |>
    dplyr::filter(date > window_start, sport_group == "Run")

  marathon_runs <- in_window |> dplyr::filter(is_marathon_stimulus)
  threshold_runs <- in_window |> dplyr::filter(is_threshold_stimulus)

  last_long <- if (nrow(marathon_runs) > 0) max(marathon_runs$date) else as.Date(NA)

  list(
    window_start = window_start,
    window_end = coros_snapshot_date,
    n_runs = nrow(in_window),
    run_miles = sum(in_window$distance_mi, na.rm = TRUE),
    run_hours = sum(in_window$duration_min, na.rm = TRUE) / 60,
    longest_mi = if (nrow(in_window) > 0) max(in_window$distance_mi, na.rm = TRUE) else 0,
    n_marathon_stimulus = nrow(marathon_runs),
    n_threshold_stimulus = nrow(threshold_runs),
    last_long_run = last_long,
    days_since_long = if (!is.na(last_long)) as.integer(coros_snapshot_date - last_long) else NA_integer_,
    marathon_runs = marathon_runs,
    threshold_runs = threshold_runs,
    # Cycling is deliberately reported separately: it feeds COROS's training
    # load and recovery, but Running Fitness is a running-only metric, so none
    # of these rides move the race predictions.
    bike_hours = sum(
      coros_activities$duration_min[coros_activities$sport_group == "Bike" &
                                      coros_activities$date > window_start],
      na.rm = TRUE
    ) / 60
  )
}

# The four ability areas COROS scores, paired with what we can observe locally.
coros_fitness_breakdown <- function() {
  p <- coros_predictor_inputs()
  best_short <- coros_activities |>
    dplyr::filter(sport_group == "Run", date > p$window_start, !is.na(pace_min_mi)) |>
    dplyr::arrange(pace_min_mi)

  data.frame(
    Ability = c("Base", "Endurance", "Speed", "Sprint"),
    `Race distance` = c("> 30 km", "~ 10 km", "~ 3 km", "~ 400 m"),
    `COROS criterion` = c("Aerobic power pace", "Threshold pace", "VO2max", "Best 30-60 s pace"),
    `Your input in window` = c(
      paste0(p$n_marathon_stimulus, " run(s) over 30 km"),
      paste0(coros_summary$threshold_pace_km[1]),
      paste0("VO2max ", coros_summary$vo2max[1]),
      if (nrow(best_short) > 0) paste0(fmt_pace(best_short$pace_min_mi[1]), " /mi best") else "--"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


# ---- timeframe filtering ---------------------------------------------------
# All ranges are anchored to the snapshot date rather than Sys.Date(), so the
# window means the same thing however stale the snapshot is.

coros_range_days <- function(range_label) {
  switch(
    range_label,
    "Last 30 days" = 30,
    "Last 3 months" = 91,
    "Last 6 months" = 182,
    "Last 12 months" = 365,
    "All time" = Inf,
    91
  )
}

coros_range_start <- function(range_label) {
  d <- coros_range_days(range_label)
  if (is.infinite(d)) as.Date("1900-01-01") else coros_snapshot_date - d
}

# Filter any date-keyed frame to the selected window.
coros_in_range <- function(df, range_label) {
  dplyr::filter(df, date > coros_range_start(range_label))
}

# What a chart can actually show for a stream, given that COROS serves
# different history depths per metric (training load is capped near 30 days by
# the API, HRV/sleep/resting HR start when the watch began recording them).
coros_stream_coverage <- function(column) {
  v <- coros_daily[[column]]
  ok <- if (column %in% c("total_min", "run_mi", "bike_mi")) !is.na(v) & v > 0 else !is.na(v)
  if (!any(ok)) return(list(n = 0, from = NA, to = NA))
  list(n = sum(ok), from = min(coros_daily$date[ok]), to = max(coros_daily$date[ok]))
}

# Human-readable note for the sidebar: what the selected window resolves to and
# how much data actually exists behind it.
coros_range_caption <- function(range_label) {
  start <- coros_range_start(range_label)
  streams <- list(
    c("Training load", "short_term_load"),
    c("Sleep HRV", "hrv_ms"),
    c("Resting HR", "resting_hr"),
    c("Sleep", "sleep_score"),
    c("Stress", "avg_stress"),
    c("Activities", "total_min")
  )
  rows <- lapply(streams, function(s) {
    cov <- coros_stream_coverage(s[2])
    shown <- sum(coros_daily$date > start &
                   (if (s[2] %in% c("total_min")) !is.na(coros_daily[[s[2]]]) & coros_daily[[s[2]]] > 0
                    else !is.na(coros_daily[[s[2]]])))
    list(name = s[1], shown = shown, total = cov$n, from = cov$from)
  })
  list(start = start, rows = rows)
}
