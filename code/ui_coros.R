# The app's tabs, all built on the COROS snapshot in data/coros/ (see
# coros_data.R).
#
# Descriptive only -- every number here is either reported by COROS or a plain
# summary of one. No models.

# Landing page: headline numbers for the snapshot year.
landing_tab <- function() {
  s <- coros_landing_stats()
  fmt <- function(x, digits = 0) format(round(x, digits), big.mark = ",", nsmall = digits)
  stat <- function(title, value, icon_name, color) {
    value_box(
      title = title,
      value = value,
      showcase = icon(icon_name),
      theme = value_box_theme(bg = "#1B2028", fg = color),
      class = "lp-stat-card",
      fill = TRUE,
      height = 175L
    )
  }

  tabPanel(
    "Home",
    value = "landing_page",
    card(
      div(
        class = "lp-hero",
        h2("Training Snapshot"),
        p(paste0("All sports, from COROS. Snapshot taken ",
                 format(coros_snapshot_date, "%B %d, %Y"), "."))
      ),
      layout_column_wrap(
        class = "lp-grid",
        width = 1 / 3,
        gap = "1rem",
        stat(paste(s$year, "Run Miles"), fmt(s$run_miles), "person-running", "#00C2FF"),
        stat(paste(s$year - 1, "Run Miles"), fmt(s$run_miles_last_year), "calendar-days", "#5FA8D3"),
        stat(paste(s$year, "Bike Miles"), fmt(s$bike_miles), "bicycle", "#F9C846"),
        stat(paste(s$year, "Avg Weekly Run Miles"), fmt(s$avg_weekly_run_miles, 1), "chart-line", "#F97316"),
        stat(paste(s$year, "Training Hours"), fmt(s$hours), "stopwatch", "#C084FC"),
        stat("Recovery", paste0(round(s$recovery_pct), "%"), "heart-pulse", "#23D18B")
      )
    )
  )
}

# Everything after the landing page, as a list so ui.R can splice it straight
# into the navbar.
coros_tabs <- function() {
  list(
    tabPanel(
      "Race Readiness",
      value = "coros_readiness",
      sidebarLayout(
        sidebarPanel(width = 2, corosInputsUI("coros")),
        mainPanel(
          width = 10,
          div(
            class = "lp-hero",
            h2("Race Readiness"),
            p(textOutput("coros_countdown_text", inline = TRUE))
          ),
          uiOutput("coros_metric_boxes"),
          div(
            class = "baseline-step",
            h4("Readiness Signals"),
            p("Each signal compares where you are now against COROS's own baseline for you."),
            uiOutput("coros_signals")
          ),
          layout_column_wrap(
            width = 1 / 2,
            gap = "1rem",
            div(
              class = "baseline-step",
              h4("COROS Race Predictions"),
              p("From COROS's fitness assessment, based on your recent training."),
              tableOutput("coros_predictions_table"),
              uiOutput("coros_goal_compare")
            ),
            div(
              class = "baseline-step",
              h4("Fitness Assessment"),
              p("COROS's running-specific fitness markers at the snapshot date."),
              uiOutput("coros_fitness_cards")
            )
          )
        )
      )
    ),

    tabPanel(
      "Training Load",
      value = "coros_load",
      sidebarLayout(
        sidebarPanel(width = 2, corosRangeUI("cload")),
        mainPanel(
          width = 10,
          card(
            class = "analysis-plot-card coros-plot-stack",
            girafeOutput("coros_load_plot", height = "380px"),
            girafeOutput("coros_load_ratio_plot", height = "380px")
          )
        )
      )
    ),

    tabPanel(
      "Recovery & Sleep",
      value = "coros_recovery",
      sidebarLayout(
        sidebarPanel(width = 2, corosRangeUI("crec")),
        mainPanel(
          width = 10,
      tabsetPanel(
        id = "coros_recovery_tabs",
        tabPanel(
          "HRV & Resting HR",
          card(
            class = "analysis-plot-card",
            girafeOutput("coros_hrv_plot", height = "380px"),
            girafeOutput("coros_rhr_plot", height = "380px")
          )
        ),
        tabPanel(
          "Sleep",
          card(
            class = "analysis-plot-card",
            girafeOutput("coros_sleep_plot", height = "380px"),
            girafeOutput("coros_sleep_score_plot", height = "380px")
          )
        )
      )
        )
      )
    ),

    tabPanel(
      "Training Log",
      value = "coros_running",
      sidebarLayout(
        sidebarPanel(width = 2, corosRangeUI("clog")),
        mainPanel(
          width = 10,
      tabsetPanel(
        id = "coros_running_tabs",
        tabPanel(
          "Volume by Sport",
          card(
            class = "analysis-plot-card",
            girafeOutput("coros_weekly_hours_plot", height = "380px"),
            girafeOutput("coros_weekly_miles_plot", height = "380px")
          )
        ),
        tabPanel(
          "Pace vs HR",
          card(
            class = "analysis-plot-card",
            girafeOutput("coros_pace_hr_plot", height = "600px")
          )
        ),
        tabPanel(
          "Recent Sessions",
          card(
            div(class = "fhm-callout",
                p("Every sport COROS recorded, newest first. Cycling shows speed rather than pace; strength records neither.")),
            uiOutput("coros_sport_totals"),
            tableOutput("coros_recent_runs")
          )
        )
      )
        )
      )
    ),

    tabPanel(
      "Race Predictor",
      value = "coros_predictor",
      card(
        div(
          class = "lp-hero",
          h2("COROS Race Predictor"),
          p("COROS's own race estimates, what feeds them, and how the calculation works.")
        ),
        uiOutput("coros_predictor_boxes"),
        div(
          class = "baseline-step",
          h4("How COROS Calculates This"),
          p("COROS documents its method rather than publishing a formula. These are its own stated rules."),
          uiOutput("coros_predictor_method")
        ),
        div(
          class = "baseline-step",
          h4("Your Six-Week Window"),
          p("COROS uses a rolling six-week window; anything older drops out. Each bar is a run inside it, coloured by the role COROS assigns."),
          girafeOutput("coros_predictor_window_plot", height = "400px"),
          uiOutput("coros_window_summary")
        ),
        div(
          class = "baseline-step",
          h4("Running Fitness Breakdown"),
          p("COROS scores four ability areas on a 40-100 scale, each judged by its own criterion."),
          tableOutput("coros_fitness_breakdown_table"),
          uiOutput("coros_predictor_sources")
        )
      )
    ),

    tabPanel(
      "Data & Refresh",
      value = "coros_data",
      card(
        div(
          class = "lp-hero",
          h2("COROS Data Source"),
          p("How this section gets its numbers, and how to update them.")
        ),
        div(
          class = "baseline-step",
          h4("Snapshot Status"),
          uiOutput("coros_data_status")
        ),
        div(
          class = "baseline-step",
          h4("Refreshing"),
          p("The Shiny app cannot reach the COROS MCP server at runtime, so the data here is a point-in-time export rather than a live feed."),
          tags$ol(
            class = "coros-roadmap",
            tags$li("In Claude Code with the ", tags$code("coros"), " MCP server connected, re-run the queries listed in ", tags$code("data/coros/snapshot/README.md"), "."),
            tags$li("Update the matching files under ", tags$code("data/coros/snapshot/"), "."),
            tags$li("Run ", tags$code("Rscript code/parse_coros_snapshot.R"), " to rebuild the tidy CSVs."),
            tags$li("Restart the app.")
          ),
          div(
            class = "fhm-callout",
            p(tags$strong("Note:"), " sleep and HRV are dated by wake-up day -- a value dated the 24th describes the night that ended on the morning of the 24th. Missing days are nights the watch was not worn, and are left as gaps rather than interpolated.")
          )
        )
      )
    )
  )
}
