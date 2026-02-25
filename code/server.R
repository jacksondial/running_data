# server for the app
source("init.R")

server <- function(input, output, session){
  # bs_themer()
  # bs_theme_update(theme, font_scale = NULL, preset = "flatly")
  source("exploratory_plots.R")
  source("analysis_plots.R")
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
  
  output$barplot <- renderPlot({
    barplot_fun(
      input$`bar-x_var`
      )
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

  output$riegel_output <- renderText(paste0("Your predicted time is: ", riegel_calculation(), ", over a distance of ", input$`riegel-d2`))

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
      scale_color_manual(values = running_palette)+
      theme_minimal()+
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

  output$baseline_model_compare <- renderTable({
    baseline_model_choice()$table
  })
  output$baseline_model_metrics <- renderTable({
    model_data <- baseline_model_data()
    fit <- baseline_model_fit()
    if (is.null(fit)) {
      return(data.frame(Message = "Not enough A-race blocks for baseline model."))
    }
    preds <- stats::predict(fit, newdata = model_data)
    rmse <- sqrt(mean((model_data$race_time_minutes - preds)^2))
    mae <- mean(abs(model_data$race_time_minutes - preds))
    r2 <- summary(fit)$r.squared

    data.frame(
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
      "Note: No model met the expected negative sign for volume/long run. Coefficient signs may be unstable with 4 races."
    } else {
      NULL
    }

    tags$div(
      tags$p(paste0("Interpretation uses the selected model (", model_name, "). Effects are associations, not causal.")),
      if (!is.null(sign_note)) tags$p(sign_note),
      if (!is.na(weekly_sec)) tags$p(paste0(
        "Volume effect: ", round(weekly_effect, 4), " min per mile/week. ",
        "Calculation: ", round(weekly_effect, 4), " * 10 * 60 = ",
        weekly_sec, " seconds for +10 miles/week."
      )),
      if (!is.na(effort_sec)) tags$p(paste0(
        "Relative Effort effect: ", round(effort_effect, 4), " min per point. ",
        "Calculation: ", round(effort_effect, 4), " * 60 = ",
        effort_sec, " seconds per +1 avg Relative Effort."
      )),
      if (!is.na(long_run_sec)) tags$p(paste0(
        "Avg weekly long run effect: ", round(long_run_effect, 4), " min per mile. ",
        "Calculation: ", round(long_run_effect, 4), " * 60 = ",
        long_run_sec, " seconds per +1 mile."
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
      geom_point(size = 3, color = "#004E64") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      geom_text(vjust = -0.8, size = 3) +
      labs(x = "Actual Time (minutes)", y = "Predicted Time (minutes)") +
      theme_minimal()
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
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(size = 3, color = "#1B998B") +
      geom_text(vjust = -0.8, size = 3) +
      labs(x = "Predicted Time (minutes)", y = "Residual (minutes)") +
      theme_minimal()
  })
}
