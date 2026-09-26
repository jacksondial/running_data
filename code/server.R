# server for the app
source("init.R")

server <- function(input, output, session){
  # bs_themer()
  # bs_theme_update(theme, font_scale = NULL, preset = "flatly")
  source("exploratory_plots.R")
  source("analysis_plots.R")
  source("coros_data.R")
  source("coros_plots.R")
  block_data <- reactive({
    hard_effort_pct <- input$`blocks-hard_effort_pct`
    build_block_data(hard_effort_pct = hard_effort_pct)
  })
  output$load_plot <- renderGirafe({
    load_plot_fun(input$`load-load_window`)
    })
  
  output$load_readiness_plot <- renderPlot({
    load_readiness_fun()
  })
  
  output$weekly_bar <- renderPlot({
    weekly_bar_fun(
      input$`weekly-weekly_plot_type`
    )
  })
  
  output$corr_plot <- renderPlot({
    corr_plot(
      input$`corr-x_var`,
      input$`corr-y_var`,
      input$`corr-group_var`
    )
  })
  
  riegel_calculation <- reactive({    
    req(input$`riegel-t1`, input$`riegel-d1`, input$`riegel-d2`)  # Ensures inputs are available
  
    riegel_function(
      t1 = input$`riegel-t1`,
      d1 = input$`riegel-d1`,
      d2 = input$`riegel-d2`
    )
  })

  riegel_minutes <- reactive({
    req(input$`riegel-t1`, input$`riegel-d1`, input$`riegel-d2`)
    riegel_predict_minutes(
      t1 = input$`riegel-t1`,
      d1 = input$`riegel-d1`,
      d2 = input$`riegel-d2`
    )
  })

  output$riegel_output <- renderText({
    paste0("Predicted using ", input$`riegel-d1`, " benchmark over ", input$`riegel-d2`, ".")
  })

  output$riegel_pred_time <- renderText({
    format_duration_compact(riegel_minutes())
  })

  output$riegel_pred_pace <- renderText({
    goal_miles <- distance_to_miles(input$`riegel-d2`)
    pace_min <- riegel_minutes() / goal_miles
    paste0(format_duration_compact(pace_min), " /mi")
  })

  output$riegel_distance_ratio <- renderText({
    bench_miles <- distance_to_miles(input$`riegel-d1`)
    goal_miles <- distance_to_miles(input$`riegel-d2`)
    paste0(round(goal_miles / bench_miles, 2), "x")
  })

  output$riegel_details <- renderUI({
    bench_miles <- distance_to_miles(input$`riegel-d1`)
    goal_miles <- distance_to_miles(input$`riegel-d2`)
    predicted_minutes <- riegel_minutes()

    tags$div(
      tags$p(paste0("Benchmark: ", input$`riegel-t1`, " minutes over ", input$`riegel-d1`, " (", round(bench_miles, 1), " mi).")),
      tags$p(paste0("Goal: ", input$`riegel-d2`, " (", round(goal_miles, 1), " mi).")),
      tags$p(paste0("Formula: T2 = T1 * (D2 / D1)^1.06.")),
      tags$p(paste0(
        "Applied: ", round(predicted_minutes, 2),
        " minutes (", format_duration(predicted_minutes), ")."
      ))
    )
  })

  output$block_timeline_plot <- renderPlot({
    block_weekly <- block_data()$block_weekly
    ggplot(
      block_weekly,
      aes(x = weeks_to_race, y = weekly_miles, color = race_name, group = race_name)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_reverse() +
      labs(
        title = "Weekly Miles by A-Race Block",
        x = "Weeks to Race",
        y = "Weekly Miles",
        color = "Race"
      ) +
      scale_color_manual(values = palette_categorical(dplyr::n_distinct(block_weekly$race_name))) +
      theme_running_dark() +
      theme(panel.grid.minor = element_blank())
  })

  format_date <- function(x) {
    if (inherits(x, "Date")) {
      return(format(x))
    }
    format(as.Date(x, origin = "1970-01-01"))
  }

  output$block_summary_table <- renderTable({
    block_summary <- block_data()$block_summary
    block_summary |>
      dplyr::select(
        race_name,
        race_date,
        race_type,
        race_distance_miles,
        block_start_date,
        block_length_weeks,
        total_miles,
        avg_weekly_miles,
        peak_week_miles,
        long_run_miles_max,
        pct_hard_miles,
        avg_hard_pace_mile,
        avg_relative_effort,
        avg_readiness,
        ramp_rate_max,
        tuneup_race_count
      ) |>
      dplyr::mutate(
        race_date = format_date(race_date),
        block_start_date = format_date(block_start_date)
      )
  })

  baseline_model_data <- reactive({
    block_summary <- block_data()$block_summary
    model_data <- block_summary |>
      dplyr::select(
        race_name,
        race_date,
        race_time_minutes,
        avg_weekly_miles,
        avg_relative_effort,
        avg_weekly_long_run_miles
      ) |>
      tidyr::drop_na()
    model_data
  })

  baseline_model_choice <- reactive({
    model_data <- baseline_model_data()
    if (nrow(model_data) < 3) {
      return(list(table = data.frame(Message = "Need at least 3 A-race blocks for comparison."), formula = NULL, best_model = NULL))
    }

    compare_models <- list(
      "Volume Only" = race_time_minutes ~ avg_weekly_miles,
      "Volume + Rel Effort" = race_time_minutes ~ avg_weekly_miles + avg_relative_effort,
      "Volume + Avg Weekly Long Run" = race_time_minutes ~ avg_weekly_miles + avg_weekly_long_run_miles
    )

    results <- lapply(names(compare_models), function(name) {
      formula <- compare_models[[name]]
      preds <- sapply(seq_len(nrow(model_data)), function(i) {
        train <- model_data[-i, , drop = FALSE]
        test <- model_data[i, , drop = FALSE]
        fit <- stats::lm(formula, data = train)
        stats::predict(fit, newdata = test)
      })
      rmse <- sqrt(mean((model_data$race_time_minutes - preds)^2))
      mae <- mean(abs(model_data$race_time_minutes - preds))
      full_fit <- stats::lm(formula, data = model_data)
      coefs <- stats::coef(full_fit)
      volume_sign_ok <- if ("avg_weekly_miles" %in% names(coefs)) coefs["avg_weekly_miles"] < 0 else TRUE
      long_run_sign_ok <- if ("avg_weekly_long_run_miles" %in% names(coefs)) coefs["avg_weekly_long_run_miles"] < 0 else TRUE
      sign_ok <- volume_sign_ok && long_run_sign_ok
      data.frame(
        Model = name,
        LOOCV_RMSE_Min = round(rmse, 2),
        LOOCV_MAE_Min = round(mae, 2),
        Sign_OK = sign_ok
      )
    })

    table <- dplyr::bind_rows(results)
    filtered <- table |> dplyr::filter(Sign_OK)
    best_row <- if (nrow(filtered) > 0) {
      filtered |> dplyr::arrange(LOOCV_RMSE_Min, LOOCV_MAE_Min) |> dplyr::slice(1)
    } else {
      table |> dplyr::arrange(LOOCV_RMSE_Min, LOOCV_MAE_Min) |> dplyr::slice(1)
    }
    best_formula <- compare_models[[best_row$Model]]
    list(table = table, formula = best_formula, best_model = best_row$Model, sign_filtered = nrow(filtered) > 0)
  })

  baseline_model_fit <- reactive({
    model_data <- baseline_model_data()
    model_choice <- baseline_model_choice()
    if (nrow(model_data) < 3 || is.null(model_choice$formula)) {
      return(NULL)
    }
    stats::lm(model_choice$formula, data = model_data)
  })

  output$baseline_model_inputs <- renderTable({
    model_data <- baseline_model_data()
    if (nrow(model_data) == 0) {
      return(data.frame(Message = "No complete A-race blocks available yet."))
    }

    model_data |>
      dplyr::transmute(
        race_name,
        race_date = format_date(race_date),
        race_time_minutes = round(race_time_minutes, 1),
        avg_weekly_miles = round(avg_weekly_miles, 1),
        avg_relative_effort = round(avg_relative_effort, 1),
        avg_weekly_long_run_miles = round(avg_weekly_long_run_miles, 1)
      )
  })

  output$baseline_model_compare <- renderTable({
    baseline_model_choice()$table
  })
  output$baseline_model_metrics <- renderTable({
    model_data <- baseline_model_data()
    fit <- baseline_model_fit()
    if (is.null(fit)) {
      return(data.frame(Message = "Not enough A-race blocks for baseline model."))
    }
    model_choice <- baseline_model_choice()
    preds <- stats::predict(fit, newdata = model_data)
    rmse <- sqrt(mean((model_data$race_time_minutes - preds)^2))
    mae <- mean(abs(model_data$race_time_minutes - preds))
    r2 <- summary(fit)$r.squared

    data.frame(
      Selected_Model = model_choice$best_model,
      Blocks_Used = nrow(model_data),
      RMSE_Minutes = round(rmse, 2),
      MAE_Minutes = round(mae, 2),
      R2 = round(r2, 3)
    )
  })

  output$baseline_model_coefs <- renderTable({
    model_data <- baseline_model_data()
    fit <- baseline_model_fit()
    if (is.null(fit)) {
      return(data.frame(Message = "Not enough A-race blocks for baseline model."))
    }

    coefs <- as.data.frame(stats::coef(summary(fit)))
    coefs$term <- rownames(coefs)
    rownames(coefs) <- NULL
    dplyr::select(coefs, term, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)
  })

  output$baseline_model_preds <- renderTable({
    model_data <- baseline_model_data()
    fit <- baseline_model_fit()
    if (is.null(fit)) {
      return(data.frame(Message = "Not enough A-race blocks for baseline model."))
    }
    preds <- stats::predict(fit, newdata = model_data)
    data.frame(
      race_name = model_data$race_name,
      race_date = format_date(model_data$race_date),
      actual_minutes = round(model_data$race_time_minutes, 2),
      predicted_minutes = round(preds, 2),
      error_minutes = round(model_data$race_time_minutes - preds, 2)
    )
  })

  output$baseline_model_cv <- renderText({
    model_data <- baseline_model_data()
    if (nrow(model_data) < 3) {
      return("Need at least 3 A-race blocks for LOOCV.")
    }

    preds <- sapply(seq_len(nrow(model_data)), function(i) {
      train <- model_data[-i, , drop = FALSE]
      test <- model_data[i, , drop = FALSE]
      model_choice <- baseline_model_choice()
      if (is.null(model_choice$formula)) {
        return(NA_real_)
      }
      fit <- stats::lm(model_choice$formula, data = train)
      stats::predict(fit, newdata = test)
    })

    rmse <- sqrt(mean((model_data$race_time_minutes - preds)^2))
    paste0(round(rmse, 2), " minutes")
  })

  output$baseline_model_loocv <- renderTable({
    model_data <- baseline_model_data()
    if (nrow(model_data) < 3) {
      return(data.frame(Message = "Need at least 3 A-race blocks for LOOCV."))
    }

    preds <- sapply(seq_len(nrow(model_data)), function(i) {
      train <- model_data[-i, , drop = FALSE]
      test <- model_data[i, , drop = FALSE]
      model_choice <- baseline_model_choice()
      if (is.null(model_choice$formula)) {
        return(NA_real_)
      }
      fit <- stats::lm(model_choice$formula, data = train)
      stats::predict(fit, newdata = test)
    })

    data.frame(
      race_name = model_data$race_name,
      race_date = format_date(model_data$race_date),
      actual_minutes = round(model_data$race_time_minutes, 2),
      loocv_predicted_minutes = round(preds, 2),
      loocv_error_minutes = round(model_data$race_time_minutes - preds, 2)
    )
  })

  output$baseline_model_interpretation <- renderUI({
    fit <- baseline_model_fit()
    if (is.null(fit)) {
      return(tags$p("Not enough data to interpret the model yet."))
    }

    coefs <- stats::coef(fit)
    weekly_effect <- coefs["avg_weekly_miles"]
    effort_effect <- coefs["avg_relative_effort"]
    long_run_effect <- coefs["avg_weekly_long_run_miles"]

    weekly_sec <- if (!is.na(weekly_effect)) round(weekly_effect * 10 * 60, 1) else NA
    effort_sec <- if (!is.na(effort_effect)) round(effort_effect * 60, 1) else NA
    long_run_sec <- if (!is.na(long_run_effect)) round(long_run_effect * 60, 1) else NA

    model_choice <- baseline_model_choice()
    model_name <- if (is.null(model_choice$best_model)) "Selected Model" else model_choice$best_model
    sign_note <- if (!isTRUE(model_choice$sign_filtered)) {
      "No candidate model preserved the expected negative volume/long-run sign, so the chosen formula is strictly the best error fit."
    } else {
      NULL
    }

    tags$div(
      tags$p(paste0("Selected baseline: ", model_name, ".")),
      tags$p("Treat these effects as directional signals from a small sample, not causal estimates."),
      if (!is.null(sign_note)) tags$p(sign_note),
      if (!is.na(weekly_sec)) tags$p(paste0(
        "+10 average weekly miles is associated with ",
        abs(weekly_sec), " seconds ",
        ifelse(weekly_sec <= 0, "faster", "slower"),
        " race time."
      )),
      if (!is.na(effort_sec)) tags$p(paste0(
        "+1 point of average Relative Effort is associated with ",
        abs(effort_sec), " seconds ",
        ifelse(effort_sec <= 0, "faster", "slower"),
        " race time."
      )),
      if (!is.na(long_run_sec)) tags$p(paste0(
        "+1 mile in average weekly long run is associated with ",
        abs(long_run_sec), " seconds ",
        ifelse(long_run_sec <= 0, "faster", "slower"),
        " race time."
      ))
    )
  })

  output$baseline_model_fit_plot <- renderPlot({
    model_data <- baseline_model_data()
    fit <- baseline_model_fit()
    if (is.null(fit)) {
      return(NULL)
    }
    preds <- stats::predict(fit, newdata = model_data)
    plot_df <- data.frame(
      actual = model_data$race_time_minutes,
      predicted = preds,
      race_name = model_data$race_name
    )
    ggplot(plot_df, aes(x = actual, y = predicted, label = race_name)) +
      geom_point(size = 3, color = "#00C2FF") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#A9B7C6") +
      geom_text(vjust = -0.8, size = 3) +
      labs(x = "Actual Time (minutes)", y = "Predicted Time (minutes)") +
      theme_running_dark()
  })

  output$baseline_model_resid_plot <- renderPlot({
    model_data <- baseline_model_data()
    fit <- baseline_model_fit()
    if (is.null(fit)) {
      return(NULL)
    }
    preds <- stats::predict(fit, newdata = model_data)
    resid <- model_data$race_time_minutes - preds
    plot_df <- data.frame(
      predicted = preds,
      residual = resid,
      race_name = model_data$race_name
    )
    ggplot(plot_df, aes(x = predicted, y = residual, label = race_name)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#A9B7C6") +
      geom_point(size = 3, color = "#23D18B") +
      geom_text(vjust = -0.8, size = 3) +
      labs(x = "Predicted Time (minutes)", y = "Residual (minutes)") +
      theme_running_dark()
  })

  full_history_model_data <- reactive({
    file_paths <- list(
      current = "../data/marathon_prediction_current.csv",
      race_samples = "../data/marathon_prediction_race_samples.csv",
      coefficients = "../data/marathon_prediction_model_coefficients.csv"
    )

    missing_files <- file_paths[!vapply(file_paths, file.exists, logical(1))]
    if (length(missing_files) > 0) {
      missing_names <- paste(basename(unlist(missing_files)), collapse = ", ")
      return(list(
        error = paste0(
          "Missing full-history model files: ",
          missing_names,
          ". Run `Rscript code/build_marathon_activity_training_data_and_model.R` from project root."
        )
      ))
    }

    list(
      current = read.csv(file_paths$current, check.names = TRUE),
      race_samples = read.csv(file_paths$race_samples, check.names = TRUE),
      coefficients = read.csv(file_paths$coefficients, check.names = TRUE)
    )
  })

  output$fhm_current_summary <- renderUI({
    model_bundle <- full_history_model_data()
    if (!is.null(model_bundle$error)) {
      return(tags$p(model_bundle$error))
    }

    current <- model_bundle$current[1, , drop = FALSE]
    applied_delta <- round(current$delta_adjustment_applied, 2)
    delta_text <- ifelse(applied_delta >= 0, paste0("+", applied_delta), as.character(applied_delta))

    tags$div(
      tags$div(
        class = "fhm-metric-grid",
        tags$div(
          class = "fhm-metric-card",
          tags$div(class = "fhm-metric-label", "Predicted Marathon"),
          tags$div(class = "fhm-metric-value", as.character(current$predicted_marathon_hms))
        ),
        tags$div(
          class = "fhm-metric-card",
          tags$div(class = "fhm-metric-label", "Baseline (Recent Performance)"),
          tags$div(class = "fhm-metric-value", as.character(current$baseline_recent_perf_hms))
        ),
        tags$div(
          class = "fhm-metric-card",
          tags$div(class = "fhm-metric-label", "Long-History Adjustment"),
          tags$div(class = "fhm-metric-value", paste0(delta_text, " min"))
        )
      ),
      tags$div(
        class = "fhm-callout",
        tags$p(paste0(
          "Baseline recent-performance estimate: this is your marathon-equivalent fitness from recent race-like efforts. ",
          "The script finds your best recent 5K/10K/Half style efforts in rolling windows (120/180/365 days), converts each to marathon-equivalent time with Riegel, and takes a weighted blend."
        )),
        tags$p(paste0(
          "Long-history adjustment: this is the model correction on top of the baseline, trained from your past races. ",
          "It learns how lifetime mileage, past-year mileage, long-run frequency, and recent-vs-long load ratio shifted your outcomes, then applies a bounded and shrunk adjustment (here ",
          delta_text, " min)."
        ))
      )
    )
  })

  output$fhm_current_table <- renderTable({
    model_bundle <- full_history_model_data()
    if (!is.null(model_bundle$error)) {
      return(data.frame(Message = model_bundle$error))
    }

    current <- model_bundle$current |>
      dplyr::transmute(
        as_of_date,
        predicted_minutes = round(predicted_marathon_minutes, 2),
        prediction_low_hms,
        prediction_high_hms,
        total_lifetime_miles = round(total_lifetime_miles, 1),
        miles_365d = round(miles_365d, 1),
        miles_42d = round(miles_42d, 1),
        long_runs_16_365d,
        recent_to_long_load_ratio = round(recent_to_long_load_ratio, 2)
      )

    current
  })

  output$fhm_model_metrics <- renderTable({
    model_bundle <- full_history_model_data()
    if (!is.null(model_bundle$error)) {
      return(data.frame(Message = model_bundle$error))
    }

    current <- model_bundle$current[1, , drop = FALSE]
    race_samples <- model_bundle$race_samples

    data.frame(
      Race_Samples_Used = nrow(race_samples),
      Baseline_LOOCV_RMSE_Min = round(current$baseline_loocv_rmse_minutes, 2),
      Raw_Model_LOOCV_RMSE_Min = round(current$raw_model_loocv_rmse_minutes, 2),
      Stabilized_Model_LOOCV_RMSE_Min = round(current$loocv_rmse_minutes, 2),
      Stabilized_Model_LOOCV_MAE_Min = round(current$loocv_mae_minutes, 2),
      Delta_Shrink_Factor = round(current$delta_adjustment_shrink_factor, 3),
      Delta_Bounds = paste0(
        "[",
        round(current$delta_adjustment_lower_bound, 2),
        ", ",
        round(current$delta_adjustment_upper_bound, 2),
        "]"
      )
    )
  })

  output$fhm_race_samples_table <- renderTable({
    model_bundle <- full_history_model_data()
    if (!is.null(model_bundle$error)) {
      return(data.frame(Message = model_bundle$error))
    }

    model_bundle$race_samples |>
      dplyr::transmute(
        race_name,
        race_type,
        race_date = format_date(as.Date(race_date)),
        race_marathon_equiv_minutes = round(race_marathon_equiv_minutes, 2),
        loocv_pred_marathon_equiv = round(loocv_pred_marathon_equiv, 2),
        loocv_error_minutes = round(loocv_error_minutes, 2),
        recent_perf_eq_minutes_filled = round(recent_perf_eq_minutes_filled, 2),
        miles_365d = round(miles_365d, 1),
        long_runs_16_365d
      )
  })

  output$fhm_model_coefficients <- renderTable({
    model_bundle <- full_history_model_data()
    if (!is.null(model_bundle$error)) {
      return(data.frame(Message = model_bundle$error))
    }

    model_bundle$coefficients |>
      dplyr::mutate(
        estimate = round(estimate, 4),
        std_error = round(std_error, 4),
        t_value = round(t_value, 3),
        p_value = round(p_value, 4)
      )
  })

  output$fhm_loocv_plot <- renderPlot({
    model_bundle <- full_history_model_data()
    if (!is.null(model_bundle$error)) {
      return(NULL)
    }

    plot_df <- model_bundle$race_samples |>
      dplyr::filter(!is.na(loocv_pred_marathon_equiv)) |>
      dplyr::mutate(label = race_name)

    ggplot(plot_df, aes(x = race_marathon_equiv_minutes, y = loocv_pred_marathon_equiv, label = label)) +
      geom_point(size = 3, color = "#FF9F1C") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#A9B7C6") +
      geom_text(vjust = -0.8, size = 3) +
      labs(
        x = "Actual Marathon-Equivalent (min)",
        y = "LOOCV Predicted Marathon-Equivalent (min)"
      ) +
      theme_running_dark()
  })

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
