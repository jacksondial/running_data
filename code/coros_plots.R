# Plots for the COROS section.
#
# All interactive (ggiraph): every mark carries a tooltip naming the date, the
# value and its units, so the chart can be read without a legend lookup. They
# render with theme_running_dark() to match the rest of the app.
#
# Descriptive only -- no fitting, no prediction.

# COROS's own training-load verdicts, kept in its wording and given a fixed
# colour each so the load chart reads the same way the watch does.
# "Last 3 months (Jun 27 - Sep 26, 64 days with data)."
coros_window_note <- function(range_label, dates, unit = "days") {
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) return(paste0(range_label, ": no data in this window."))
  n <- length(unique(dates))
  paste0(range_label, " (", format(min(dates), "%b %d"), " - ",
         format(max(dates), "%b %d, %Y"), ", ", n, " ",
         if (n == 1) sub("s$", "", unit) else unit, " with data).")
}

coros_load_colors <- c(
  "Excessive"   = "#E84855",
  "Optimized"   = "#23D18B",
  "Maintaining" = "#5FA8D3",
  "Performance" = "#F9C846"
)

# "Other" covers hikes, walks, skis -- real training time, so it gets a colour
# rather than falling through to a grey NA in the legend.
coros_sport_colors <- c(
  "Run"      = "#00C2FF",
  "Bike"     = "#F9C846",
  "Strength" = "#6C4AB6",
  "Other"    = "#5B7083"
)

coros_status_colors <- c(good = "#23D18B", watch = "#F9C846", flag = "#E84855")

# One girafe wrapper so every COROS chart hovers and styles identically.
# Bar width is in DATE units, so it must not exceed the spacing of the data:
# one day for daily bars, seven for weekly ones. Scaling it with the window
# (as an earlier version did) makes bars overlap and position_stack() warn on
# long ranges. On a year-long view 1-day bars render as a dense band, which is
# the correct read of daily data at that zoom.
coros_bar_width <- function(range_label, weekly = FALSE) {
  if (weekly) 6 else 0.9
}

coros_girafe <- function(p, height_svg = 3.1, width_svg = 12) {
  girafe(
    ggobj = p,
    width_svg = width_svg,
    height_svg = height_svg,
    options = list(
      opts_hover(css = "stroke-width:3;opacity:1;"),
      opts_hover_inv(css = "opacity:0.25;"),
      opts_tooltip(css = "
        background-color: #141822;
        color: #E6E6E6;
        border: 1px solid #3A434F;
        border-radius: 10px;
        padding: 9px 11px;
        font-size: 13px;
        line-height: 1.45;
        box-shadow: 0 4px 14px rgba(0,0,0,0.45);
      "),
      opts_zoom(max = 6),
      # Fill the card width instead of sitting in a fixed-aspect letterbox.
      opts_sizing(rescale = TRUE, width = 1),
      opts_toolbar(saveaspng = TRUE, position = "topright")
    )
  )
}

# ---- training load ---------------------------------------------------------

coros_load_plot <- function(range_label = "Last 3 months") {
  dat <- coros_daily |>
    coros_in_range(range_label) |>
    dplyr::filter(!is.na(short_term_load)) |>
    dplyr::select(date, short_term_load, long_term_load, load_ratio,
                  load_comment, total_min, run_mi, bike_mi)

  long <- dat |>
    tidyr::pivot_longer(c(short_term_load, long_term_load),
                        names_to = "series", values_to = "load") |>
    dplyr::mutate(series = dplyr::recode(
      series,
      short_term_load = "Short-term (fatigue)",
      long_term_load = "Long-term (fitness)"
    ))

  tip <- glue::glue(
    "Date: {format(dat$date, '%b %d, %Y')}
     Short-term load (fatigue): {round(dat$short_term_load)}
     Long-term load (fitness): {round(dat$long_term_load)}
     Ratio: {sprintf('%.2f', dat$load_ratio)} - {dat$load_comment}
     That day: {round(dat$run_mi, 1)} mi run, {round(dat$bike_mi, 1)} mi bike"
  )

  p <- ggplot(long, aes(x = date, y = load, color = series)) +
    geom_line_interactive(aes(group = series), linewidth = 1.1) +
    geom_point_interactive(
      data = dat, inherit.aes = FALSE,
      aes(x = date, y = short_term_load, tooltip = tip, data_id = date),
      size = 2.4, color = "#00C2FF"
    ) +
    geom_point_interactive(
      data = dat, inherit.aes = FALSE,
      aes(x = date, y = long_term_load, tooltip = tip, data_id = date),
      size = 2.4, color = "#23D18B"
    ) +
    scale_color_manual(values = c("Short-term (fatigue)" = "#00C2FF",
                                  "Long-term (fitness)" = "#23D18B")) +
    labs(
      title = "Training Load: short-term vs long-term",
      subtitle = paste0(coros_window_note(range_label, dat$date),
                        " Short-term above long-term means you are loading."),
      x = NULL, y = "COROS Load", color = NULL
    ) +
    theme_running_dark() +
    theme(legend.position = "top")

  coros_girafe(p)
}

coros_load_ratio_plot <- function(range_label = "Last 3 months") {
  dat <- coros_daily |> coros_in_range(range_label) |> dplyr::filter(!is.na(load_ratio))

  dat$tip <- glue::glue(
    "Date: {format(dat$date, '%b %d, %Y')}
     Load ratio: {sprintf('%.2f', dat$load_ratio)}
     COROS verdict: {dat$load_comment}
     Short-term {round(dat$short_term_load)} / long-term {round(dat$long_term_load)}
     Training that day: {round(dat$total_min)} min"
  )

  p <- ggplot(dat, aes(x = date, y = load_ratio)) +
    annotate("rect", xmin = min(dat$date), xmax = max(dat$date),
             ymin = 0.8, ymax = 1.3, fill = "#23D18B", alpha = 0.10) +
    geom_hline(yintercept = 1, color = "#6B7785", linetype = "dashed") +
    geom_line(color = "#8FA3B8", linewidth = 0.7) +
    geom_point_interactive(
      aes(fill = load_comment, tooltip = tip, data_id = date),
      shape = 21, size = 3.4, color = "#1E242C", stroke = 0.6
    ) +
    scale_fill_manual(values = coros_load_colors, name = "COROS verdict") +
    labs(
      title = "Load Ratio with COROS's daily verdict",
      subtitle = paste0(coros_window_note(range_label, dat$date),
                        " Shaded band is the productive range."),
      x = NULL, y = "Short-term / long-term"
    ) +
    theme_running_dark() +
    theme(legend.position = "top")

  coros_girafe(p)
}

# ---- recovery & sleep ------------------------------------------------------

coros_hrv_plot <- function(range_label = "Last 3 months") {
  dat <- coros_daily |> coros_in_range(range_label) |> dplyr::filter(!is.na(hrv_ms))

  dat$tip <- glue::glue(
    "Night ending: {format(dat$date, '%b %d, %Y')}
     Sleep HRV: {round(dat$hrv_ms)} ms - {dat$hrv_status}
     Your normal range: {dat$hrv_low}-{dat$hrv_high} ms
     Baseline: {dat$hrv_baseline} ms"
  )

  p <- ggplot(dat, aes(x = date, y = hrv_ms)) +
    geom_ribbon(aes(ymin = hrv_low, ymax = hrv_high), fill = "#23D18B", alpha = 0.13) +
    geom_line(aes(y = hrv_baseline), color = "#23D18B", linetype = "dashed", linewidth = 0.7) +
    geom_line(color = "#8FA3B8", linewidth = 0.7) +
    geom_point_interactive(
      aes(fill = hrv_status, tooltip = tip, data_id = date),
      shape = 21, size = 3.6, color = "#1E242C", stroke = 0.6
    ) +
    scale_fill_manual(
      values = c("Normal" = "#23D18B", "Above normal" = "#5FA8D3",
                 "Below normal" = "#F9C846", "Low" = "#E84855"),
      name = NULL
    ) +
    labs(
      title = "Sleep HRV against your normal range",
      subtitle = paste0(coros_window_note(range_label, dat$date),
                        " Band and baseline are COROS's own."),
      x = NULL, y = "HRV (ms)"
    ) +
    theme_running_dark() +
    theme(legend.position = "top")

  coros_girafe(p)
}

coros_rhr_plot <- function(range_label = "Last 3 months") {
  dat <- coros_daily |> coros_in_range(range_label) |> dplyr::filter(!is.na(resting_hr))
  avg <- mean(dat$resting_hr, na.rm = TRUE)

  dat$tip <- glue::glue(
    "Date: {format(dat$date, '%b %d, %Y')}
     Resting HR: {round(dat$resting_hr)} bpm
     vs {round(avg, 1)} bpm average ({sprintf('%+.1f', dat$resting_hr - avg)})"
  )

  p <- ggplot(dat, aes(x = date, y = resting_hr)) +
    geom_hline(yintercept = avg, color = "#6B7785", linetype = "dashed") +
    annotate("text", x = min(dat$date), y = avg, vjust = -0.6, hjust = 0,
             label = paste0("avg ", round(avg, 1), " bpm"),
             color = "#9DB0C4", size = 3.4) +
    geom_line(color = "#00C2FF", linewidth = 0.9) +
    geom_point_interactive(aes(tooltip = tip, data_id = date),
                           color = "#00C2FF", size = 2.8) +
    labs(
      title = "Resting Heart Rate",
      subtitle = paste0(coros_window_note(range_label, dat$date),
                        " A sustained drift upward is the classic warning sign."),
      x = NULL, y = "bpm"
    ) +
    theme_running_dark()

  coros_girafe(p)
}

coros_sleep_plot <- function(range_label = "Last 3 months") {
  base <- coros_daily |> coros_in_range(range_label) |> dplyr::filter(!is.na(sleep_score))

  dat <- base |>
    dplyr::mutate(
      Deep = sleep_hours * deep_pct / 100,
      Light = sleep_hours * light_pct / 100,
      REM = sleep_hours * rem_pct / 100,
      Awake = sleep_hours * awake_pct / 100
    ) |>
    tidyr::pivot_longer(c(Deep, Light, REM, Awake),
                        names_to = "stage", values_to = "stage_hours") |>
    dplyr::mutate(
      stage = factor(stage, levels = c("Awake", "REM", "Light", "Deep")),
      tip = glue::glue(
        "Night ending: {format(date, '%b %d, %Y')}
         {stage}: {sprintf('%.1f', stage_hours)} h
         Total sleep: {sprintf('%.1f', sleep_hours)} h
         Sleep score: {round(sleep_score)}/100"
      )
    )

  p <- ggplot(dat, aes(x = date, y = stage_hours, fill = stage)) +
    geom_col_interactive(aes(tooltip = tip, data_id = paste(date, stage)),
                         width = coros_bar_width(range_label)) +
    geom_hline(yintercept = 8, color = "#6B7785", linetype = "dashed") +
    scale_fill_manual(values = c(Deep = "#0B4F6C", Light = "#5FA8D3",
                                 REM = "#6C4AB6", Awake = "#4A5462")) +
    labs(
      title = "Sleep duration and composition",
      subtitle = paste0(coros_window_note(range_label, dat$date),
                        " Dashed line at 8 h; gaps are nights the watch was not worn."),
      x = NULL, y = "Hours", fill = NULL
    ) +
    theme_running_dark() +
    theme(legend.position = "top")

  coros_girafe(p)
}

coros_sleep_score_plot <- function(range_label = "Last 3 months") {
  dat <- coros_daily |> coros_in_range(range_label) |> dplyr::filter(!is.na(sleep_score))
  dat$tip <- glue::glue(
    "Night ending: {format(dat$date, '%b %d, %Y')}
     Sleep score: {round(dat$sleep_score)}/100
     Duration: {sprintf('%.1f', dat$sleep_hours)} h
     Deep {dat$deep_pct}% / Light {dat$light_pct}% / REM {dat$rem_pct}%"
  )

  p <- ggplot(dat, aes(x = date, y = sleep_score)) +
    geom_col_interactive(aes(fill = sleep_score, tooltip = tip, data_id = date),
                         width = coros_bar_width(range_label)) +
    scale_fill_gradient(low = "#E84855", high = "#23D18B", guide = "none") +
    geom_hline(yintercept = 80, color = "#6B7785", linetype = "dashed") +
    labs(
      title = "Nightly sleep score",
      subtitle = paste0(coros_window_note(range_label, dat$date),
                        " COROS's own 0-100 score, dashed line at 80."),
      x = NULL, y = "Score"
    ) +
    theme_running_dark()

  coros_girafe(p)
}

# ---- training across sports ------------------------------------------------

# Hours, not miles: 200 mi on the bike and 20 mi running are not the same unit,
# but both cost time and both show up in COROS's load and recovery numbers.
coros_weekly_hours_plot <- function(range_label = "Last 3 months") {
  dat <- coros_weekly_sport |>
    dplyr::filter(week_monday > coros_range_start(range_label)) |>
    dplyr::mutate(
      sport_group = factor(sport_group, levels = c("Other", "Strength", "Bike", "Run")),
      tip = glue::glue(
        "Week of {format(week_monday, '%b %d, %Y')}
         {sport_group}: {sprintf('%.1f', hours)} h across {sessions} session(s)
         {dplyr::if_else(sport_group == 'Strength', 'no distance recorded',
                         paste0(round(miles, 1), ' mi'))}"
      )
    )

  p <- ggplot(dat, aes(x = week_monday, y = hours, fill = sport_group)) +
    geom_col_interactive(aes(tooltip = tip, data_id = paste(week_monday, sport_group)),
                         width = coros_bar_width(range_label, weekly = TRUE)) +
    scale_fill_manual(values = coros_sport_colors, name = NULL) +
    labs(
      title = "Weekly training time by sport",
      subtitle = paste0(coros_window_note(range_label, dat$week_monday, "weeks"),
                        " Hours is the cross-sport currency."),
      x = "Week beginning", y = "Hours"
    ) +
    theme_running_dark() +
    theme(legend.position = "top")

  coros_girafe(p)
}

coros_weekly_miles_plot <- function(range_label = "Last 3 months") {
  dat <- coros_weekly |>
    dplyr::filter(week_monday > coros_range_start(range_label)) |>
    dplyr::mutate(tip = glue::glue(
      "Week of {format(week_monday, '%b %d, %Y')}
       Running: {round(run_miles, 1)} mi in {sprintf('%.1f', run_hours)} h
       Cycling: {round(bike_miles, 1)} mi in {sprintf('%.1f', bike_hours)} h
       Longest run: {round(longest_run_mi, 1)} mi
       {sessions} sessions total"
    ))

  p <- ggplot(dat, aes(x = week_monday)) +
    geom_col_interactive(aes(y = run_miles, tooltip = tip, data_id = week_monday),
                         fill = "#00C2FF", width = coros_bar_width(range_label, weekly = TRUE), alpha = 0.9) +
    geom_line_interactive(aes(y = longest_run_mi), color = "#F9C846", linewidth = 0.8) +
    geom_point_interactive(aes(y = longest_run_mi, tooltip = tip, data_id = week_monday),
                           color = "#F9C846", size = 2.8) +
    labs(
      title = "Weekly running miles",
      subtitle = paste0(coros_window_note(range_label, dat$week_monday, "weeks"),
                        " Bars are run miles; gold line is the week's longest run."),
      x = "Week beginning", y = "Miles"
    ) +
    theme_running_dark()

  coros_girafe(p)
}

# Running efficiency only -- bike pace is speed-derived and not comparable, so
# rides are excluded here rather than mixed into the same axis.
coros_pace_hr_plot <- function(range_label = "Last 3 months") {
  dat <- coros_activities |>
    coros_in_range(range_label) |>
    dplyr::filter(sport_group == "Run", !is.na(avg_hr), !is.na(pace_min_mi), distance_mi >= 1) |>
    dplyr::mutate(tip = glue::glue(
      "{format(date, '%b %d, %Y')} - {sport}
       Pace: {vapply(pace_min_mi, fmt_pace, character(1))} /mi
       Avg HR: {avg_hr} bpm
       Distance: {round(distance_mi, 2)} mi in {duration}"
    ))

  p <- ggplot(dat, aes(x = avg_hr, y = pace_min_mi)) +
    geom_point_interactive(
      aes(size = distance_mi, color = date, tooltip = tip, data_id = paste(date, distance_mi)),
      alpha = 0.85
    ) +
    scale_y_reverse(labels = function(x) vapply(x, fmt_pace, character(1))) +
    scale_color_gradient(low = "#4A5462", high = "#00C2FF", trans = "date", name = "Date") +
    scale_size_continuous(range = c(2, 9), name = "Miles") +
    labs(
      title = "Pace vs heart rate (running only)",
      subtitle = paste0(coros_window_note(range_label, dat$date),
                        " Up and to the left is better: faster pace at the same heart rate."),
      x = "Average HR (bpm)", y = "Average pace (min/mi)"
    ) +
    theme_running_dark()

  coros_girafe(p, height_svg = 4.9)
}

# ---- race predictor --------------------------------------------------------

# Which sessions inside COROS's six-week window are the ones it says drive the
# marathon estimate, and how the window is filling up.
coros_predictor_window_plot <- function() {
  p_in <- coros_predictor_inputs()

  dat <- coros_activities |>
    dplyr::filter(sport_group == "Run", date > p_in$window_start) |>
    dplyr::mutate(
      role = dplyr::case_when(
        is_marathon_stimulus ~ "Marathon stimulus (>30 km)",
        is_threshold_stimulus ~ "Threshold stimulus",
        TRUE ~ "Other running"
      ),
      tip = glue::glue(
        "{format(date, '%b %d, %Y')} - {sport}
         Distance: {round(distance_mi, 2)} mi ({round(distance_km, 1)} km)
         Pace: {vapply(pace_min_mi, fmt_pace, character(1))} /mi
         Duration: {duration}
         Counts as: {role}"
      )
    )

  p <- ggplot(dat, aes(x = date, y = distance_mi)) +
    geom_hline(yintercept = 30 * 0.621371, color = "#F9C846", linetype = "dashed") +
    annotate("text", x = min(dat$date), y = 30 * 0.621371, vjust = -0.7, hjust = 0,
             label = "30 km - COROS's marathon-stimulus threshold",
             color = "#F9C846", size = 3.3) +
    geom_segment_interactive(
      aes(xend = date, y = 0, yend = distance_mi, color = role, tooltip = tip, data_id = paste(date, distance_mi)),
      linewidth = 1.6
    ) +
    geom_point_interactive(
      aes(color = role, tooltip = tip, data_id = paste(date, distance_mi)), size = 2.8
    ) +
    scale_color_manual(
      values = c("Marathon stimulus (>30 km)" = "#23D18B",
                 "Threshold stimulus" = "#F9C846",
                 "Other running" = "#4A6072"),
      name = NULL
    ) +
    labs(
      title = "What is inside COROS's six-week prediction window",
      subtitle = paste0("Runs since ", format(p_in$window_start, "%b %d"),
                        ". Anything older has aged out of the calculation."),
      x = NULL, y = "Miles"
    ) +
    theme_running_dark() +
    theme(legend.position = "top")

  coros_girafe(p, height_svg = 3.3)
}
