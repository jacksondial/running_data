# UI for app
source("inputs.R")
source("landing_page.R")
source("init.R", local = TRUE)

# future step would be to update to use bslib::page_navbar instead of navbarPage
ui <- page_navbar(
  # titlePanel("Running Shiny App"),
  tags$head(
    tags$style(HTML("
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
  title = "Title",
  id = "title",
    tabPanel(
      "Landing Page",
      value = "landing_page",
      card(
      fluidRow(
        column(total_miles_vb, width = 4),
        column(miles_25_vb, width = 4),
        column(miles_24_vb, width = 4)
        )
      )
    ),
    tabPanel(
      "Exploration",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          conditionalPanel(
            condition = "input.tabs == 'Barplot'",
            barInputsUI("bar")
          ),
          conditionalPanel(
            condition = "input.tabs == 'Correlation Plot'",
            corrInputsUI("corr")
          )
        ),
        mainPanel(
          # Nested tabsetPanel for barplot and correlation plot
          tabsetPanel(
            id = "tabs",
            tabPanel(
              "Weekly Mileage",
              fluidRow(
                plotOutput("weekly_bar")
              )
            ),
            tabPanel(
              "Boring Barplot",
              fluidRow(
                plotOutput("barplot")
              )
            ),
            tabPanel(
              "Correlation Plot",
              fluidRow(
                plotOutput("corr_plot")
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
        # Uncomment this when "siegelInputs" is defined
        riegelInputsUI("riegel")
      ),
      mainPanel(
        tabsetPanel(
          id = "prediction_tabs", # Add an ID for the tabsetPanel
          tabPanel(
            "Riegel Method",
            fluidRow(
              # h3("Riegel Method Output"),
              textOutput("riegel_output") # Placeholder for Riegel method output
            )
          ),
          tabPanel(
            "Method 2",
            fluidRow(
              h3("Method 2 Output"),
              textOutput("method2_output") # Placeholder for Method 2 output
            )
          )
        )
      )
    )
  )
  
)







