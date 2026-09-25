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
    duration = readr::col_character(),
    avg_pace_km = readr::col_character(),
    is_workout = readr::col_logical(),
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
    miles_prior_7d = sum(
      coros_daily$run_mi[coros_daily$date <= max(coros_daily$date) - 7 &
                           coros_daily$date > max(coros_daily$date) - 14],
      na.rm = TRUE
    )
  )
})

# ---- weekly running volume -------------------------------------------------

coros_weekly <- coros_activities |>
  dplyr::mutate(week_monday = as.Date(cut(date, "week", start.on.monday = TRUE))) |>
  dplyr::group_by(week_monday) |>
  dplyr::summarise(
    miles = sum(distance_mi, na.rm = TRUE),
    hours = sum(duration_min, na.rm = TRUE) / 60,
    runs = dplyr::n(),
    longest_mi = max(distance_mi, na.rm = TRUE),
    .groups = "drop"
  ) |>
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

  # Volume trend -- taper check
  vol_change <- (cur$miles_7d - cur$miles_prior_7d) / cur$miles_prior_7d * 100
  add(
    "Volume Trend",
    paste0(round(cur$miles_7d, 1), " mi"),
    if (isTRUE(vol_change <= 5)) "good" else "watch",
    paste0(fmt_signed(vol_change, 0, "%"), " vs the week before (",
           round(cur$miles_prior_7d, 1), " mi).")
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
