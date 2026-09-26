# Strava section of the navbar.
#
# Holds every analysis built on the Strava export (`app_dat`): training load,
# exploration, race prediction, and marathon block comparisons. The panels here
# were the original top-level tabs of the app and are unchanged apart from now
# living under the "Strava" navbar menu.
#
# Inner tabsetPanel ids ("analysis_tabs", "exp_tabs", "prediction_tabs",
# "blocks_tabs") are referenced by conditionalPanel conditions and by server.R,
# so they must stay stable.

strava_nav <- function() {
  nav_menu(
    "Strava",
    value = "strava_section",
    icon = icon("strava"),
    tabPanel(
      "Load & Training Status",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          conditionalPanel(
            condition = "input.analysis_tabs == 'Training Load'",
            loadInputsUI("load")
            ),
          ),
        mainPanel(
          tabsetPanel(
            id = "analysis_tabs",
            tabPanel(
              "Training Load",
              card(
                class = "analysis-plot-card",
                girafeOutput("load_plot", width = "100%", height = "620px")
              )                      
            ),
            tabPanel(
              "Load & Readiness",
              card(
                class = "analysis-plot-card",
                plotOutput("load_readiness_plot", height = "620px")
              )
            )
          )
        )
      )
    ),
      tabPanel(
        "Exploration",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            conditionalPanel(
              condition = "input.exp_tabs == 'Correlation Plot'",
              corrInputsUI("corr")
            ),
            conditionalPanel(
              condition = "input.exp_tabs == 'Weekly Mileage'",
              weeklyInputsUI("weekly")
            )
          ),
          mainPanel(
            # Nested tabsetPanel for barplot and correlation plot
            tabsetPanel(
              id = "exp_tabs",
              tabPanel(
                "Weekly Mileage",
                card(
                  class = "analysis-plot-card",
                  fluidRow(
                    plotOutput("weekly_bar", height = "620px")
                  )
                )
              ),
              tabPanel(
                "Correlation Plot",
                card(
                  class = "analysis-plot-card",
                  fluidRow(
                    plotOutput("corr_plot", height = "620px")
                  )
                )
              )
            )
          )
        )
    ),
    tabPanel(
      "Prediction",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          # numericInput(
          #   "placeholder",
          #   "Placeholder",
          #   value = 5,
          #   min = 0,
          #   max = 10
          # )
          # Uncomment this when "riegelInputs" is defined
          riegelInputsUI("riegel")
        ),
        mainPanel(
          tabsetPanel(
            id = "prediction_tabs", # Add an ID for the tabsetPanel
            tabPanel(
              "Riegel Method",
              div(
                class = "riegel-shell",
                div(
                  class = "riegel-hero",
                  h3("Riegel Race Time Predictor"),
                  p("Estimate finish times across race distances using your current benchmark performance.")
                ),
                layout_column_wrap(
                  width = 1 / 3,
                  gap = "1rem",
                  card(
                    class = "riegel-metric-card",
                    div(class = "riegel-metric-label", "Predicted Finish"),
                    div(class = "riegel-metric-value", textOutput("riegel_pred_time")),
                    div(class = "riegel-metric-sub", textOutput("riegel_output"))
                  ),
                  card(
                    class = "riegel-metric-card",
                    div(class = "riegel-metric-label", "Estimated Pace"),
                    div(class = "riegel-metric-value", textOutput("riegel_pred_pace")),
                    div(class = "riegel-metric-sub", "Projected average pace at selected goal distance.")
                  ),
                  card(
                    class = "riegel-metric-card",
                    div(class = "riegel-metric-label", "Distance Multiplier"),
                    div(class = "riegel-metric-value", textOutput("riegel_distance_ratio")),
                    div(class = "riegel-metric-sub", "How much farther your goal event is vs. your benchmark.")
                  )
                ),
                div(
                  class = "riegel-details",
                  h5("Calculation Details"),
                  uiOutput("riegel_details")
                )
              )
            ),
            tabPanel(
              "Baseline Model",
              fluidRow(
                h3("Baseline Model (A-Race Blocks)"),
                div(
                  class = "baseline-step",
                  h4("1. Start With Completed A-Race Blocks"),
                  p("Each row is one finished block and the race result it produced. These are the observations available to the baseline model."),
                  tableOutput("baseline_model_inputs")
                ),
                div(
                  class = "baseline-step",
                  h4("2. Compare Simple Candidate Models"),
                  p("Test a few small formulas first, then keep the one with the best leave-one-out error profile."),
                  tableOutput("baseline_model_compare")
                ),
                div(
                  class = "baseline-step",
                  h4("3. Fit The Best Baseline Model"),
                  p("Summarize the selected model, the main directional takeaways, and the overall fit on the available A-race blocks."),
                  uiOutput("baseline_model_interpretation"),
                  tableOutput("baseline_model_metrics")
                ),
                div(
                  class = "baseline-step",
                  h4("4. Check Race-Level Performance"),
                  p("A final sanity check against actual race results to see where the baseline is close and where it drifts."),
                  plotOutput("baseline_model_fit_plot", height = "320px"),
                  tableOutput("baseline_model_loocv")
                )
              )
            ),
            tabPanel(
              "Full-History Model",
              fluidRow(
                h3("Full-History Marathon Model"),
                div(
                  class = "baseline-step",
                  h4("1. Current Prediction"),
                  p("Prediction uses recent benchmark-equivalent performance plus long-term training history from your full run archive."),
                  uiOutput("fhm_current_summary"),
                  tableOutput("fhm_current_table")
                ),
                div(
                  class = "baseline-step",
                  h4("2. Model Diagnostics"),
                  p("Error metrics compare baseline recent-performance estimates to the long-history adjusted model."),
                  tableOutput("fhm_model_metrics")
                ),
                div(
                  class = "baseline-step",
                  h4("3. Race-Level Validation"),
                  p("Validation uses leave-one-race-out predictions against marathon-equivalent race outcomes."),
                  plotOutput("fhm_loocv_plot", height = "320px"),
                  tableOutput("fhm_race_samples_table")
                ),
                div(
                  class = "baseline-step",
                  h4("4. Adjustment Coefficients"),
                  p("These coefficients describe how long-term features adjust the recent-performance baseline."),
                  tableOutput("fhm_model_coefficients")
                )
              )
            )
          )
        )
      )
    )
    ,
    tabPanel(
      "Marathon Blocks",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          helpText("A-race training blocks with Relative Effort as the hard-effort signal."),
          sliderInput(
            "blocks-hard_effort_pct",
            "Hard Effort Percentile",
            min = 0.5,
            max = 0.95,
            value = 0.75,
            step = 0.05
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "blocks_tabs",
            tabPanel(
              "Block Timeline",
              fluidRow(
                plotOutput("block_timeline_plot", height = "500px")
              )
            ),
            tabPanel(
              "Block Summary",
              fluidRow(
                tableOutput("block_summary_table")
              )
            )
          )
        )
      )
    )
  )
}
