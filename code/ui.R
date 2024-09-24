# UI for app
source("inputs.R")
source("landing_page.R")
ui <- fluidPage( 
  titlePanel("Running Shiny App"),
  tags$head(
    tags$style(HTML("
      .small-box {
        font-size: 24px;
        background-color: #00a65a !important;
        color: white !important;
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
  tabsetPanel(
    tabPanel(
      "Landing Page",
      fluidRow(),
      fluidRow(
        total_miles_vb,
        miles_24_vb
        )
    ),
  
  tabPanel(
    "Exploration",
    sidebarLayout(
      sidebarPanel(
        sharedInputsUI("explore")
      ),
      mainPanel(
        fluidRow(
          plotOutput("barplot")
        )
      )
    )
  )
  )
)