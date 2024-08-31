# server for the app

server <- function(input, output, session){
  source("plots.R")
  output$barplot <- renderPlot({
    barplot_fun(input$x_var)
  })
}