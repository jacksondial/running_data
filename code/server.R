# server for the app
source("init.R")

server <- function(input, output, session){
  source("plots.R")
  output$barplot <- renderPlot({
    barplot_fun(input$`bar-x_var`)
  })
  
  output$corr_plot <- renderPlot({
    corr_plot(
      input$`corr-x_var`,
      input$`corr-y_var`,
      input$`corr-group_var`
    )
  })
  
}