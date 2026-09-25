# Plots for the COROS section.
#
# All of these read the snapshot data prepared in coros_data.R and render with
# theme_running_dark() so they match the rest of the app. Descriptive only --
# no fitting, no prediction.

# COROS's own training-load verdicts, kept in its wording and given a fixed
# colour each so the load chart reads the same way the watch does.
coros_load_colors <- c(
  "Excessive"   = "#E84855",
  "Optimized"   = "#23D18B",
  "Maintaining" = "#5FA8D3",
  "Performance" = "#F9C846"
)

coros_status_colors <- c(good = "#23D18B", watch = "#F9C846", flag = "#E84855")

# ---- training load ---------------------------------------------------------

coros_load_plot <- function() {
  dat <- coros_daily |>
    dplyr::filter(!is.na(short_term_load)) |>
    dplyr::select(date, short_term_load, long_term_load, load_comment)

  long <- dat |>
    tidyr::pivot_longer(
      c(short_term_load, long_term_load),
      names_to = "series", values_to = "load"
    ) |>
    dplyr::mutate(series = dplyr::recode(
      series,
      short_term_load = "Short-term (fatigue)",
      long_term_load = "Long-term (fitness)"
    ))

  ggplot(long, aes(x = date, y = load, color = series)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.8) +
    scale_color_manual(values = c(
      "Short-term (fatigue)" = "#00C2FF",
      "Long-term (fitness)" = "#23D18B"
    )) +
    labs(
      title = "Training Load: short-term vs long-term",
      subtitle = "Short-term above long-term means you are loading; below means you are absorbing it.",
      x = NULL, y = "COROS Load", color = NULL
    ) +
    theme_running_dark() +
    theme(legend.position = "top")
}

coros_load_ratio_plot <- function() {
  dat <- coros_daily |> dplyr::filter(!is.na(load_ratio))

  ggplot(dat, aes(x = date, y = load_ratio)) +
    # COROS treats roughly 0.8-1.3 as productive; outside it is under/overreaching
    annotate("rect", xmin = min(dat$date), xmax = max(dat$date),
             ymin = 0.8, ymax = 1.3, fill = "#23D18B", alpha = 0.10) +
    geom_hline(yintercept = 1, color = "#6B7785", linetype = "dashed") +
    geom_line(color = "#8FA3B8", linewidth = 0.7) +
    geom_point(aes(fill = load_comment), shape = 21, size = 3.4,
               color = "#1E242C", stroke = 0.6) +
    scale_fill_manual(values = coros_load_colors, name = "COROS verdict") +
    labs(
      title = "Load Ratio with COROS's daily verdict",
      subtitle = "Shaded band is the productive range. Each point is coloured by what COROS called that day.",
      x = NULL, y = "Short-term / long-term"
    ) +
    theme_running_dark() +
    theme(legend.position = "top")
}

# ---- recovery & sleep ------------------------------------------------------

coros_hrv_plot <- function() {
  dat <- coros_daily |> dplyr::filter(!is.na(hrv_ms))

  ggplot(dat, aes(x = date, y = hrv_ms)) +
    geom_ribbon(aes(ymin = hrv_low, ymax = hrv_high),
                fill = "#23D18B", alpha = 0.13) +
    geom_line(aes(y = hrv_baseline), color = "#23D18B",
              linetype = "dashed", linewidth = 0.7) +
    geom_line(color = "#8FA3B8", linewidth = 0.7) +
    geom_point(aes(fill = hrv_status), shape = 21, size = 3.6,
               color = "#1E242C", stroke = 0.6) +
    scale_fill_manual(
      values = c("Normal" = "#23D18B", "Above normal" = "#5FA8D3",
                 "Below normal" = "#F9C846", "Low" = "#E84855"),
      name = NULL
    ) +
    labs(
      title = "Sleep HRV against your normal range",
      subtitle = "Band and dashed baseline are COROS's own, not recomputed. Dated by wake-up day.",
      x = NULL, y = "HRV (ms)"
    ) +
    theme_running_dark() +
    theme(legend.position = "top")
}

coros_rhr_plot <- function() {
  dat <- coros_daily |> dplyr::filter(!is.na(resting_hr))
  avg <- mean(dat$resting_hr, na.rm = TRUE)

  ggplot(dat, aes(x = date, y = resting_hr)) +
    geom_hline(yintercept = avg, color = "#6B7785", linetype = "dashed") +
    annotate("text", x = min(dat$date), y = avg, vjust = -0.6, hjust = 0,
             label = paste0("avg ", round(avg, 1), " bpm"),
             color = "#9DB0C4", size = 3.4) +
    geom_line(color = "#00C2FF", linewidth = 0.9) +
    geom_point(color = "#00C2FF", size = 2.6) +
    labs(
      title = "Resting Heart Rate",
      subtitle = "A drift upward in the last taper week is the classic early warning sign.",
      x = NULL, y = "bpm"
    ) +
    theme_running_dark()
}

coros_sleep_plot <- function() {
  dat <- coros_daily |>
    dplyr::filter(!is.na(sleep_score)) |>
    dplyr::mutate(
      Deep = sleep_hours * deep_pct / 100,
      Light = sleep_hours * light_pct / 100,
      REM = sleep_hours * rem_pct / 100,
      Awake = sleep_hours * awake_pct / 100
    ) |>
    tidyr::pivot_longer(c(Deep, Light, REM, Awake),
                        names_to = "stage", values_to = "hours") |>
    dplyr::mutate(stage = factor(stage, levels = c("Awake", "REM", "Light", "Deep")))

  ggplot(dat, aes(x = date, y = hours, fill = stage)) +
    geom_col(width = 0.75) +
    geom_hline(yintercept = 8, color = "#6B7785", linetype = "dashed") +
    scale_fill_manual(values = c(
      Deep = "#0B4F6C", Light = "#5FA8D3", REM = "#6C4AB6", Awake = "#4A5462"
    )) +
    labs(
      title = "Sleep duration and composition",
      subtitle = "Dashed line at 8 h. Dated by wake-up day; missing bars are nights the watch was not worn.",
      x = NULL, y = "Hours", fill = NULL
    ) +
    theme_running_dark() +
    theme(legend.position = "top")
}

coros_sleep_score_plot <- function() {
  dat <- coros_daily |> dplyr::filter(!is.na(sleep_score))

  ggplot(dat, aes(x = date, y = sleep_score)) +
    geom_col(aes(fill = sleep_score), width = 0.75) +
    scale_fill_gradient(low = "#E84855", high = "#23D18B", guide = "none") +
    geom_hline(yintercept = 80, color = "#6B7785", linetype = "dashed") +
    labs(
      title = "Nightly sleep score",
      subtitle = "COROS's own 0-100 score. Dashed line at 80.",
      x = NULL, y = "Score"
    ) +
    theme_running_dark()
}

# ---- running ---------------------------------------------------------------

coros_weekly_plot <- function() {
  dat <- coros_weekly

  ggplot(dat, aes(x = week_monday, y = miles)) +
    geom_col(fill = "#00C2FF", width = 5.5, alpha = 0.85) +
    geom_point(aes(y = longest_mi), color = "#F9C846", size = 2.8) +
    geom_line(aes(y = longest_mi), color = "#F9C846", linewidth = 0.7) +
    labs(
      title = "Weekly running volume",
      subtitle = "Bars are weekly miles; the gold line is that week's longest single run.",
      x = "Week beginning", y = "Miles"
    ) +
    theme_running_dark()
}

coros_pace_hr_plot <- function() {
  dat <- coros_activities |>
    dplyr::filter(!is.na(avg_hr), !is.na(pace_min_mi), distance_mi >= 1)

  ggplot(dat, aes(x = avg_hr, y = pace_min_mi)) +
    geom_point(aes(size = distance_mi, color = date), alpha = 0.85) +
    scale_y_reverse(labels = function(x) vapply(x, fmt_pace, character(1))) +
    scale_color_gradient(low = "#4A5462", high = "#00C2FF",
                         trans = "date", name = "Date") +
    scale_size_continuous(range = c(2, 9), name = "Miles") +
    labs(
      title = "Pace vs heart rate",
      subtitle = "Up and to the left is better: faster pace for the same heart rate. Runs of 1 mi or more.",
      x = "Average HR (bpm)", y = "Average pace (min/mi)"
    ) +
    theme_running_dark()
}
