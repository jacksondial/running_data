# UI for app
source("inputs.R")
source("landing_page.R")
source("init.R", local = TRUE)


dark_theme <- bs_theme(
  version = 5,
  bg = "#0E1117",
  fg = "#E6E6E6",
  primary = "#FFD700",
  secondary = "#004E64",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)


ui <- page_navbar(
  # theme = "styles.css", # this does not work atm
  # titlePanel("Running Shiny App"),
  # bs_theme_update(theme, font_scale = NULL, preset = "flatly"),
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    bg = "#0B0F14",
    fg = "#E6EEF5",
    primary = "#00C2FF",
    secondary = "#1F2A37",
    success = "#23D18B",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    font_scale = 1.1
  ),
  
  # theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      body {
        background: #0B0F14;
        color: #E6EEF5;
      }
      h1, h2, h3, h4, h5, h6, p, label, .navbar-brand, .nav-link, .form-label {
        color: #E6EEF5 !important;
      }
      .navbar, .navbar-dark {
        background-color: #0F1720 !important;
        border-bottom: 1px solid #1F2A37;
      }
      .card {
        background-color: #111827;
        border: 1px solid #1F2A37;
        box-shadow: 0 10px 30px rgba(0,0,0,0.25);
      }
      .card-title, .card-text {
        color: #E6EEF5 !important;
      }
      .form-control, .selectize-input, .selectize-dropdown, .form-select {
        background-color: #0F1720 !important;
        color: #E6EEF5 !important;
        border: 1px solid #1F2A37 !important;
      }
      .selectize-dropdown-content {
        background-color: #0F1720;
      }
      .value-box {
        border: 1px solid #1F2A37;
      }
      .lp-hero {
        background: linear-gradient(130deg, #0F1720 0%, #111827 55%, #102030 100%);
        border: 1px solid #1F2A37;
        border-radius: 16px;
        padding: 24px 28px;
        margin-bottom: 18px;
      }
      .lp-hero h2 {
        margin-bottom: 6px;
        font-weight: 700;
      }
      .lp-hero p {
        color: #A9B7C6 !important;
        margin-bottom: 0;
      }
      .lp-grid {
        margin-top: 8px;
      }
      .lp-stat-card {
        border: 1px solid #253447 !important;
        border-radius: 14px !important;
        box-shadow: 0 10px 22px rgba(0, 0, 0, 0.22);
      }
      .lp-stat-card .value-box-title {
        font-size: 0.9rem;
        letter-spacing: 0.04em;
        text-transform: uppercase;
      }
      .lp-stat-card .value-box-value {
        font-size: 2rem;
        font-weight: 700;
      }
      .lp-stat-card--readiness .value-box-value {
        font-size: 1.45rem;
      }
      .analysis-plot-card {
        min-height: 700px;
      }
      .riegel-shell {
        padding: 10px 8px;
      }
      .riegel-hero {
        background: linear-gradient(130deg, #0F1720 0%, #101D2B 45%, #122334 100%);
        border: 1px solid #1F2A37;
        border-radius: 16px;
        padding: 22px 26px;
        margin-bottom: 16px;
      }
      .riegel-hero h3 {
        margin-bottom: 6px;
        font-weight: 700;
      }
      .riegel-hero p {
        margin-bottom: 0;
        color: #A9B7C6 !important;
      }
      .riegel-metric-card {
        background-color: #0F1720 !important;
        border: 1px solid #1F2A37 !important;
        border-radius: 14px !important;
        min-height: 150px;
      }
      .riegel-metric-label {
        color: #9DB0C4 !important;
        font-size: 0.8rem;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        margin-bottom: 8px;
      }
      .riegel-metric-value {
        font-size: 2rem;
        font-weight: 700;
        color: #E6EEF5;
      }
      .riegel-metric-sub {
        color: #9AA4B2 !important;
        margin-top: 8px;
      }
      .riegel-details {
        background-color: #0F1720;
        border: 1px solid #1F2A37;
        border-radius: 14px;
        padding: 18px 20px;
        margin-top: 14px;
      }
      .riegel-details h5 {
        margin-bottom: 12px;
      }
      .riegel-details p {
        color: #C8D3DF !important;
        margin-bottom: 6px;
      }
      .nav-tabs .nav-link {
        color: #A9B7C6 !important;
      }
      .nav-tabs .nav-link.active {
        background-color: #111827;
        border-color: #1F2A37 #1F2A37 #111827;
        color: #E6EEF5 !important;
      }
      .text-muted {
        color: #9AA4B2 !important;
      }
      .table {
        color: #E6EEF5;
      }
      .table thead th {
        background-color: #0F1720;
        border-bottom: 1px solid #1F2A37;
      }
      .table tbody tr {
        border-color: #1F2A37;
      }
      .small-box {
        font-size: 24px;
        background-color: #00a65a !important;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.1);
      }
      .small-box h3 {
        font-size: 34px;
        font-weight: bold;
        margin: 0;
      }
      .small-box p {
        font-size: 18px;
        margin-bottom: 10px;
      }
      .small-box .icon {
        font-size: 50px;
        margin-left: 10px;
      }
      
    "))
  ),
  title = "Exercise Data",
  id = "title",
    tabPanel(
      "Landing Page",
      value = "landing_page",
      card(
        div(
          class = "lp-hero",
          h2("Running Snapshot"),
          p("Quick look at training progression, annual volume, and current readiness.")
        ),
        layout_column_wrap(
          class = "lp-grid",
          width = 1 / 3,
          gap = "1rem",
          lifetime_miles_vb,
          miles_this_year_vb,
          miles_last_year_vb,
          runs_this_year_vb,
          avg_weekly_this_year_vb,
          longest_run_this_year_vb,
          readiness_box
        )
      )
    ),
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
              h4("Interpretation"),
              uiOutput("baseline_model_interpretation"),
              br(),
              h4("Model Comparison (LOOCV)"),
              tableOutput("baseline_model_compare"),
              br(),
              h4("Model Metrics"),
              tableOutput("baseline_model_metrics"),
              br(),
              h4("Coefficients"),
              tableOutput("baseline_model_coefs"),
              br(),
              h4("Actual vs Predicted"),
              plotOutput("baseline_model_fit_plot", height = "300px"),
              br(),
              h4("Residuals"),
              plotOutput("baseline_model_resid_plot", height = "300px"),
              br(),
              h4("Predictions (Training Fit)"),
              tableOutput("baseline_model_preds"),
              br(),
              h4("Leave-One-Out CV Error (min/mile)"),
              textOutput("baseline_model_cv"),
              br(),
              h4("Leave-One-Out Predictions"),
              tableOutput("baseline_model_loocv")
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
