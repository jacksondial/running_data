
sharedInputsUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      ns("x_var"),
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
  )
}
  
  