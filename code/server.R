# Server for the app.
source("init.R")

server <- function(input, output, session){
  # ---- COROS section -------------------------------------------------------
  # Everything below is descriptive: COROS-reported values, or plain summaries
  # of them. The race date is the only user input that feeds any of it.

  coros_race_date <- reactive({
    req(input$`coros-race_date`)
    as.Date(input$`coros-race_date`)
  })

  coros_days_to_race <- reactive({
    as.integer(coros_race_date() - Sys.Date())
  })

  output$coros_countdown_text <- renderText({
    d <- coros_days_to_race()
    when <- if (d > 1) paste0(d, " days out") else if (d == 1) "tomorrow" else
      if (d == 0) "race day" else paste0(abs(d), " days ago")
    paste0(
      "Race ", when, " (", format(coros_race_date(), "%B %d, %Y"), "). ",
      "COROS snapshot taken ", format(coros_snapshot_date, "%B %d, %Y"), "."
    )
  })

  coros_box <- function(title, value, sub, color) {
    div(
      class = "coros-metric",
      div(class = "coros-metric-label", title),
      div(class = "coros-metric-value", style = paste0("color:", color, ";"), value),
      div(class = "coros-metric-sub", sub)
    )
  }

  output$coros_metric_boxes <- renderUI({
    cur <- coros_current
    d <- coros_days_to_race()

    div(
      class = "coros-metric-grid",
      coros_box(
        "Days to Race",
        if (d >= 0) d else "--",
        format(coros_race_date(), "%b %d"),
        "#00C2FF"
      ),
      coros_box(
        "Recovery",
        paste0(round(coros_summary$recovery_pct[1]), "%"),
        coros_summary$recovery_level[1],
        if (coros_summary$recovery_pct[1] >= 80) "#23D18B" else "#F9C846"
      ),
      coros_box(
        "Load Ratio",
        sprintf("%.2f", cur$load_ratio),
        paste0("COROS: ", cur$load_comment),
        if (identical(cur$load_comment, "Excessive")) "#E84855" else "#23D18B"
      ),
      coros_box(
        "Sleep HRV",
        paste0(round(cur$hrv), " ms"),
        paste0("baseline ", round(cur$hrv_baseline), " ms"),
        if (isTRUE(cur$hrv >= cur$hrv_low)) "#23D18B" else "#F9C846"
      ),
      coros_box(
        "Resting HR",
        paste0(round(cur$resting_hr), " bpm"),
        paste0(fmt_signed(cur$resting_hr - cur$rhr_baseline, 1), " vs 3-wk avg"),
        if (isTRUE(cur$resting_hr - cur$rhr_baseline <= 3)) "#23D18B" else "#F9C846"
      ),
      coros_box(
        "Sleep (7d)",
        round(cur$sleep_score_7d),
        paste0(sprintf("%.1f", cur$sleep_hours_7d), " h per night"),
        if (isTRUE(cur$sleep_score_7d >= 80)) "#23D18B" else "#F9C846"
      )
    )
  })

  output$coros_signals <- renderUI({
    sig <- coros_readiness_signals()
    labels <- c(good = "On track", watch = "Watch", flag = "Flag")

    rows <- lapply(seq_len(nrow(sig)), function(i) {
      row <- sig[i, ]
      div(
        class = paste0("coros-signal coros-signal--", row$status),
        div(
          class = "coros-signal-head",
          span(class = "coros-signal-metric", row$metric),
          span(class = "coros-signal-value", row$value),
          span(class = paste0("coros-badge coros-badge--", row$status),
               labels[[row$status]])
        ),
        div(class = "coros-signal-note", row$note)
      )
    })
    do.call(tagList, rows)
  })

  output$coros_predictions_table <- renderTable({
    coros_predictions()
  }, striped = TRUE, width = "100%")

  output$coros_goal_compare <- renderUI({
    goal <- trimws(input$`coros-goal_time`)
    parts <- suppressWarnings(as.numeric(strsplit(goal, ":", fixed = TRUE)[[1]]))
    pred_parts <- as.numeric(strsplit(coros_summary$pred_marathon[1], ":", fixed = TRUE)[[1]])

    if (length(parts) != 3 || anyNA(parts)) {
      return(div(class = "fhm-callout", p("Enter a goal time as h:mm:ss to compare against the COROS marathon prediction.")))
    }

    goal_min <- parts[1] * 60 + parts[2] + parts[3] / 60
    pred_min <- pred_parts[1] * 60 + pred_parts[2] + pred_parts[3] / 60
    gap <- pred_min - goal_min

    verdict <- if (gap <= 0) {
      paste0("COROS has you ", fmt_pace(abs(gap)), " faster than your goal.")
    } else {
      paste0("COROS has you ", fmt_pace(gap), " slower than your goal.")
    }

    div(
      class = "fhm-callout",
      p(tags$strong(paste0("vs your goal of ", goal, ": ")), verdict),
      p(class = "text-muted",
        "COROS predicts from recent training load and pace, not from a course, the weather, or your pacing plan.")
    )
  })

  output$coros_fitness_cards <- renderUI({
    thresh_km <- sub(" /km", "", coros_summary$threshold_pace_km[1])
    tk <- as.numeric(strsplit(thresh_km, ":", fixed = TRUE)[[1]])
    thresh_mi <- fmt_pace((tk[1] + tk[2] / 60) / 0.621371)

    div(
      class = "fhm-metric-grid",
      div(class = "fhm-metric-card",
          div(class = "fhm-metric-label", "VO2max"),
          div(class = "fhm-metric-value", coros_summary$vo2max[1])),
      div(class = "fhm-metric-card",
          div(class = "fhm-metric-label", "Running Level"),
          div(class = "fhm-metric-value", coros_summary$running_level[1])),
      div(class = "fhm-metric-card",
          div(class = "fhm-metric-label", "Threshold Pace"),
          div(class = "fhm-metric-value", paste0(thresh_mi, " /mi")),
          div(class = "coros-metric-sub", paste0(thresh_km, " /km"))),
      div(class = "fhm-metric-card",
          div(class = "fhm-metric-label", "7-Day Volume"),
          div(class = "fhm-metric-value", paste0(round(coros_current$miles_7d, 1), " mi")))
    )
  })

  # ---- shared timeframe ----------------------------------------------------
  # Shiny needs unique input ids, so each chart tab renders its own selector.
  # These observers keep the three in step, which makes the choice feel like one
  # shared control rather than three independent ones. The identical() guard
  # stops them ping-ponging.
  coros_range_ids <- c("cload", "crec", "clog")

  # One source of truth. Whichever selector the user touches writes here, and
  # the others are updated to match, so the three read as a single control.
  coros_range_rv <- reactiveVal("Last 3 months")

  for (this_id in coros_range_ids) {
    local({
      src <- this_id
      observeEvent(input[[paste0(src, "-range")]], {
        val <- input[[paste0(src, "-range")]]
        if (is.null(val)) return()
        if (!identical(coros_range_rv(), val)) coros_range_rv(val)
        for (dst in setdiff(coros_range_ids, src)) {
          if (!identical(input[[paste0(dst, "-range")]], val)) {
            updateSelectInput(session, paste0(dst, "-range"), selected = val)
          }
        }
      }, ignoreInit = TRUE)
    })
  }

  coros_range <- reactive(coros_range_rv())

  # Sidebar note: what the window resolves to, and how much data each stream
  # actually has behind it (COROS serves different depths per metric).
  coros_range_note_ui <- function() {
    renderUI({
      cap <- coros_range_caption(coros_range())
      rows <- lapply(cap$rows, function(r) {
        short <- r$shown < r$total
        div(
          class = "coros-cov-row",
          span(class = "coros-cov-name", r$name),
          span(class = paste0("coros-cov-n", if (r$shown == 0) " coros-cov-none" else ""),
               paste0(r$shown, if (short) paste0(" / ", r$total) else "", " d"))
        )
      })
      tagList(
        div(class = "coros-cov-title", "Days with data in view"),
        div(rows),
        div(class = "coros-cov-foot",
            "Training load is capped near 30 days by the COROS API; HRV, sleep and resting HR begin Dec 2025.")
      )
    })
  }
  output$`cload-range_note` <- coros_range_note_ui()
  output$`crec-range_note` <- coros_range_note_ui()
  output$`clog-range_note` <- coros_range_note_ui()

  # All COROS charts are ggiraph, so every mark carries a hover tooltip.
  output$coros_load_plot <- renderGirafe({ coros_load_plot(coros_range()) })
  output$coros_load_ratio_plot <- renderGirafe({ coros_load_ratio_plot(coros_range()) })
  output$coros_hrv_plot <- renderGirafe({ coros_hrv_plot(coros_range()) })
  output$coros_rhr_plot <- renderGirafe({ coros_rhr_plot(coros_range()) })
  output$coros_sleep_plot <- renderGirafe({ coros_sleep_plot(coros_range()) })
  output$coros_sleep_score_plot <- renderGirafe({ coros_sleep_score_plot(coros_range()) })
  output$coros_weekly_hours_plot <- renderGirafe({ coros_weekly_hours_plot(coros_range()) })
  output$coros_weekly_miles_plot <- renderGirafe({ coros_weekly_miles_plot(coros_range()) })
  output$coros_pace_hr_plot <- renderGirafe({ coros_pace_hr_plot(coros_range()) })
  output$coros_predictor_window_plot <- renderGirafe({ coros_predictor_window_plot() })

  output$coros_recent_runs <- renderTable({
    coros_activities |>
      coros_in_range(coros_range()) |>
      dplyr::arrange(dplyr::desc(date)) |>
      utils::head(25) |>
      dplyr::transmute(
        Date = format(date, "%b %d"),
        Sport = sport,
        Location = location,
        Time = duration,
        Miles = dplyr::if_else(is.na(distance_mi), NA_character_,
                               format(round(distance_mi, 2), nsmall = 2)),
        # Runs get pace, rides get speed -- one column each rather than forcing
        # both sports into a single misleading number.
        `Pace /mi` = dplyr::if_else(sport_group == "Run" & !is.na(pace_min_mi),
                                    vapply(pace_min_mi, fmt_pace, character(1)), NA_character_),
        `Speed mph` = dplyr::if_else(sport_group == "Bike" & !is.na(speed_mph),
                                     format(round(speed_mph, 1), nsmall = 1), NA_character_),
        `Avg HR` = avg_hr,
        Cal = calories
      )
  }, striped = TRUE, width = "100%", na = "-")

  output$coros_sport_totals <- renderUI({
    tot <- coros_activities |>
      coros_in_range(coros_range()) |>
      dplyr::group_by(sport_group) |>
      dplyr::summarise(
        n = dplyr::n(),
        hours = sum(duration_min, na.rm = TRUE) / 60,
        miles = sum(distance_mi, na.rm = TRUE),
        .groups = "drop"
      )

    cards <- lapply(seq_len(nrow(tot)), function(i) {
      r <- tot[i, ]
      div(
        class = "fhm-metric-card",
        div(class = "fhm-metric-label", r$sport_group),
        div(class = "fhm-metric-value", paste0(sprintf("%.1f", r$hours), " h")),
        div(class = "coros-metric-sub",
            paste0(r$n, " sessions",
                   if (r$miles > 0) paste0(" - ", round(r$miles), " mi") else ""))
      )
    })
    div(class = "fhm-metric-grid", cards)
  })

  output$coros_data_status <- renderUI({
    streams <- list(
      c("Training load", "short_term_load"),
      c("Resting HR", "resting_hr"),
      c("Sleep HRV", "hrv_ms"),
      c("Sleep", "sleep_score"),
      c("Stress", "avg_stress")
    )

    cards <- lapply(streams, function(s) {
      vals <- coros_daily[[s[2]]]
      have <- sum(!is.na(vals))
      dates <- coros_daily$date[!is.na(vals)]
      div(
        class = "fhm-metric-card",
        div(class = "fhm-metric-label", s[1]),
        div(class = "fhm-metric-value", paste0(have, " days")),
        div(class = "coros-metric-sub",
            if (have > 0) paste(format(min(dates), "%b %d"), "-", format(max(dates), "%b %d")) else "no data")
      )
    })

    tagList(
      div(class = "fhm-metric-grid", cards),
      div(
        class = "fhm-metric-grid",
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Runs"),
            div(class = "fhm-metric-value", nrow(coros_activities)),
            div(class = "coros-metric-sub",
                paste(format(min(coros_activities$date), "%b %d"), "-",
                      format(max(coros_activities$date), "%b %d")))),
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Snapshot Date"),
            div(class = "fhm-metric-value", format(coros_snapshot_date, "%b %d")),
            div(class = "coros-metric-sub",
                paste0(as.integer(Sys.Date() - coros_snapshot_date), " days old")))
      )
    )
  })

  # ---- COROS race predictor ------------------------------------------------

  output$coros_predictor_boxes <- renderUI({
    preds <- list(
      list("5K", coros_summary$pred_5k[1], "#5FA8D3"),
      list("10K", coros_summary$pred_10k[1], "#00C2FF"),
      list("Half Marathon", coros_summary$pred_half[1], "#F9C846"),
      list("Marathon", coros_summary$pred_marathon[1], "#23D18B")
    )
    boxes <- lapply(preds, function(x) {
      coros_box(x[[1]], x[[2]], "COROS estimate", x[[3]])
    })
    div(class = "coros-metric-grid", boxes)
  })

  output$coros_predictor_method <- renderUI({
    tagList(
      div(
        class = "fhm-metric-grid",
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Data window"),
            div(class = "fhm-metric-value", "6 weeks"),
            div(class = "coros-metric-sub", "older training ages out")),
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Running Fitness"),
            div(class = "fhm-metric-value", coros_summary$running_level[1]),
            div(class = "coros-metric-sub", "on COROS's 40-100 scale")),
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "VO2max"),
            div(class = "fhm-metric-value", coros_summary$vo2max[1]),
            div(class = "coros-metric-sub", "feeds the ~3 km Speed score")),
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Threshold pace"),
            div(class = "fhm-metric-value", sub(" /km", "", coros_summary$threshold_pace_km[1])),
            div(class = "coros-metric-sub", "per km - feeds Endurance"))
      ),
      tags$ul(
        class = "coros-roadmap",
        tags$li(tags$strong("Pace against heart rate, not distance scaling. "),
                "COROS estimates fitness from the pace-to-heart-rate relationship in your runs, rather than scaling one race result up with a Riegel-style exponent."),
        tags$li(tags$strong("Distance-specific. "),
                "Long runs beyond 30 km move the marathon estimate specifically, while a 60-minute threshold run moves the 10K and half estimates."),
        tags$li(tags$strong("Four ability areas. "),
                "Running Fitness decomposes into Base (>30 km), Endurance (~10 km), Speed (~3 km) and Sprint (~400 m), each judged by its own criterion."),
        tags$li(tags$strong("Pace-Duration Model. "),
                "Pace zones come from fitting Maximal Speed, Anaerobic Work and Critical Speed, the last of which sits close to threshold pace."),
        tags$li(tags$strong("Ideal conditions assumed. "),
                "COROS states the predictions assume ideal weather and course conditions, so they are a fitness ceiling rather than a race-day forecast.")
      ),
      div(
        class = "fhm-callout",
        p(tags$strong("Cycling does not move these numbers."),
          " Running Fitness is running-only, so your rides feed COROS's training load, recovery and HRV, but not the race predictor. That cuts both ways: the aerobic base from cycling is real, and the predictor cannot see it.")
      )
    )
  })

  output$coros_window_summary <- renderUI({
    p <- coros_predictor_inputs()

    long_note <- if (is.na(p$days_since_long)) {
      "No run over 30 km inside the window - the marathon estimate is leaning on shorter running."
    } else {
      paste0("Last run over 30 km was ", p$days_since_long,
             " days ago (", format(p$last_long_run, "%b %d"), ").")
    }

    tagList(
      div(
        class = "fhm-metric-grid",
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Runs in window"),
            div(class = "fhm-metric-value", p$n_runs),
            div(class = "coros-metric-sub", paste0(round(p$run_miles), " mi / ", sprintf("%.1f", p$run_hours), " h"))),
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Over 30 km"),
            div(class = "fhm-metric-value", p$n_marathon_stimulus),
            div(class = "coros-metric-sub", "drives the marathon estimate")),
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Threshold sessions"),
            div(class = "fhm-metric-value", p$n_threshold_stimulus),
            div(class = "coros-metric-sub", "drive 10K and half")),
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Longest run"),
            div(class = "fhm-metric-value", paste0(round(p$longest_mi, 1), " mi")),
            div(class = "coros-metric-sub", "inside the window")),
        div(class = "fhm-metric-card",
            div(class = "fhm-metric-label", "Cycling in window"),
            div(class = "fhm-metric-value", paste0(sprintf("%.0f", p$bike_hours), " h")),
            div(class = "coros-metric-sub", "not counted by the predictor"))
      ),
      div(class = "fhm-callout", p(long_note))
    )
  })

  output$coros_fitness_breakdown_table <- renderTable({
    coros_fitness_breakdown()
  }, striped = TRUE, width = "100%")

  output$coros_predictor_sources <- renderUI({
    div(
      class = "fhm-callout",
      p(tags$strong("Sources"), " - COROS's own documentation and reporting on it:"),
      tags$ul(
        class = "coros-roadmap",
        tags$li(tags$a(href = "https://coros.com/stories/coros-metrics/c/introducing-running-fitness",
                       target = "_blank", rel = "noopener", "COROS - Introducing Running Fitness"),
                " (40-100 scale, the four ability areas, six-week window)"),
        tags$li(tags$a(href = "https://support.coros.com/hc/en-us/articles/360061452651-EvoLab-Metrics",
                       target = "_blank", rel = "noopener", "COROS Help Center - EvoLab Metrics"),
                " (Pace-Duration Model, threshold pace)"),
        tags$li(tags$a(href = "https://coros.com/stories/coros-coaches/c/5-features-to-know-for-your-next-road-race",
                       target = "_blank", rel = "noopener", "COROS - Race day watch features"),
                " (what the Race Predictor widget shows)"),
        tags$li(tags$a(href = "https://www.gneta.app/blog/race-predictors-compared",
                       target = "_blank", rel = "noopener", "Gneta - Race Predictors Compared"),
                " (six-week rolling window, distance-specific behaviour vs Garmin/Polar)")
      ),
      p(class = "text-muted",
        "COROS has not published an exact formula. Everything above is its documented behaviour, not a reverse-engineered model, and nothing in this app recomputes the predictions.")
    )
  })
}
