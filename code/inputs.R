
barInputsUI <- function(id){
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

corrInputsUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      ns("x_var"),
      "X-Variable",
      selected = "Elapsed.Time",
      choices = c(
        "Elapsed.Time",
        "Distance",
        "Max.Heart.Rate",
        "Relative.Effort",
        "Calories"
      )
    ),
    selectInput(
      ns("y_var"),
      "Y-Variable",
      selected = "Distance",
      choices = c(
        "Elapsed.Time",
        "Distance",
        "Max.Heart.Rate",
        "Relative.Effort",
        "Calories"
      )
    ),
    selectInput(
      ns("group_var"),
      "Grouping Variable",
      selected = "month",
      choices = c(
        "day",
        "month",
        "year"
      )
    )
  )
}
  