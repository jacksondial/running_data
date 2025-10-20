# server for the app
source("init.R")

server <- function(input, output, session){
  source("plots.R")
  output$barplot <- renderPlot({
    barplot_fun(input$`bar-x_var`)
  })
  
  output$weekly_bar <- renderPlot({
    weekly_bar_fun()
  })
  
  output$corr_plot <- renderPlot({
    corr_plot(
      input$`corr-x_var`,
      input$`corr-y_var`,
      input$`corr-group_var`
    )
  })
  
  riegel_calculation <- reactive({    
    req(input$`riegel-t1`, input$`riegel-d1`, input$`riegel-d2`)  # Ensures inputs are available
  
    riegel_function(
      t1 = input$`riegel-t1`,
      d1 = input$`riegel-d1`,
      d2 = input$`riegel-d2`
    )
  })

  output$riegel_output <- renderText(paste0("Your predicted time is: ", riegel_calculation(), ", over a distance of ", input$`riegel-d1`))
}