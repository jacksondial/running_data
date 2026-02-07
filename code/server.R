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
      theme_minimal()
  })

  output$block_summary_table <- renderTable({
    block_summary <- block_data()$block_summary
    block_summary |>
      dplyr::select(
        race_name,
        race_date,
        block_start_date,
        block_length_weeks,
        total_miles,
        avg_weekly_miles,
        peak_week_miles,
        long_run_miles_max,
        pct_hard_miles,
        avg_relative_effort,
        avg_readiness,
        ramp_rate_max,
        tuneup_race_count
      )
  })

  output$baseline_model_table <- renderTable({
    block_summary <- block_data()$block_summary
    model_data <- block_summary |>
      dplyr::select(
        race_time_minutes,
        avg_weekly_miles,
        peak_week_miles,
        pct_hard_miles
      ) |>
      tidyr::drop_na()

    if (nrow(model_data) < 3) {
      return(data.frame(Message = "Not enough A-race blocks for baseline model."))
    }

    fit <- stats::lm(
      race_time_minutes ~ avg_weekly_miles + peak_week_miles + pct_hard_miles,
      data = model_data
    )

    coefs <- as.data.frame(stats::coef(summary(fit)))
    coefs$term <- rownames(coefs)
    rownames(coefs) <- NULL
    dplyr::select(coefs, term, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)
  })

  output$baseline_model_cv <- renderText({
    block_summary <- block_data()$block_summary
    model_data <- block_summary |>
      dplyr::select(
        race_time_minutes,
        avg_weekly_miles,
        peak_week_miles,
        pct_hard_miles
      ) |>
      tidyr::drop_na()

    if (nrow(model_data) < 3) {
      return("Need at least 3 A-race blocks for LOOCV.")
    }

    preds <- sapply(seq_len(nrow(model_data)), function(i) {
      train <- model_data[-i, , drop = FALSE]
      test <- model_data[i, , drop = FALSE]
      fit <- stats::lm(
        race_time_minutes ~ avg_weekly_miles + peak_week_miles + pct_hard_miles,
        data = train
      )
      stats::predict(fit, newdata = test)
    })

    rmse <- sqrt(mean((model_data$race_time_minutes - preds)^2))
    paste0(round(rmse, 2), " minutes")
  })
}
