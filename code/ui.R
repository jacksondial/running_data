# UI for app

ui <- fluidPage(
  titlePanel("Running Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "x_var",
        "Choose X-Variable:",
        selected = "Elapsed.Time",
        choices = c(
          "Elapsed.Time",
          "Distance",
          "Max.Heart.Rate",
          "Relative.Effort",
          "Calories"
        )
      )
    ),
    mainPanel(
      fluidRow(
        plotOutput("barplot")
      )
    )
  )
  
)