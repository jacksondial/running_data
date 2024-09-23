# server for the app
source("init.R")

server <- function(input, output, session){
  source("plots.R")
  output$barplot <- renderPlot({
    barplot_fun(input$`explore-x_var`)
  })
}