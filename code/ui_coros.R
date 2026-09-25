# COROS section of the navbar.
#
# Built on the point-in-time snapshot in data/coros/ (see coros_data.R). The
# framing throughout is race-week readiness rather than season-long history:
# COROS's value over the Strava export is its recovery/sleep/HRV signals, and
# those matter most in a taper.
#
# Descriptive only -- every number here is either reported by COROS or a plain
# summary of one. No models.

coros_nav <- function() {
  nav_menu(
    "COROS",
    value = "coros_section",
    icon = icon("mountain-sun"),

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
      card(
        class = "analysis-plot-card",
        plotOutput("coros_load_plot", height = "330px"),
        plotOutput("coros_load_ratio_plot", height = "330px")
      )
    ),

    tabPanel(
      "Recovery & Sleep",
      value = "coros_recovery",
      tabsetPanel(
        id = "coros_recovery_tabs",
        tabPanel(
          "HRV & Resting HR",
          card(
            class = "analysis-plot-card",
            plotOutput("coros_hrv_plot", height = "330px"),
            plotOutput("coros_rhr_plot", height = "330px")
          )
        ),
        tabPanel(
          "Sleep",
          card(
            class = "analysis-plot-card",
            plotOutput("coros_sleep_plot", height = "330px"),
            plotOutput("coros_sleep_score_plot", height = "330px")
          )
        )
      )
    ),

    tabPanel(
      "Running Log",
      value = "coros_running",
      tabsetPanel(
        id = "coros_running_tabs",
        tabPanel(
          "Volume",
          card(
            class = "analysis-plot-card",
            plotOutput("coros_weekly_plot", height = "620px")
          )
        ),
        tabPanel(
          "Pace vs HR",
          card(
            class = "analysis-plot-card",
            plotOutput("coros_pace_hr_plot", height = "620px")
          )
        ),
        tabPanel(
          "Recent Runs",
          card(tableOutput("coros_recent_runs"))
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
