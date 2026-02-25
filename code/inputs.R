
loadInputsUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      ns("load_window"),
      "Choose Time Frame",
      selected = "3 Months",
      choices = c(
        "Week",
        "Month",
        "3 Months",
        "6 Months",
        "1 Year",
        "2 Years",
        "All"
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
      selected = "distance_miles",
      choices = c(
        "distance_miles",
        "elapsed_minutes",
        "moving_minutes",
        "avg_speed_mph",
        "avg_pace_mile",
        "Average.Speed",
        "Max.Speed",
        "Elevation.Gain",
        "Elevation.Loss",
        "Average.Heart.Rate",
        "Max.Heart.Rate",
        "Relative.Effort",
        "Training.Load",
        "Average.Watts",
        "Weighted.Average.Power",
        "Calories"
      )
    ),
    selectInput(
      ns("y_var"),
      "Y-Variable",
      selected = "elapsed_minutes",
      choices = c(
        "distance_miles",
        "elapsed_minutes",
        "moving_minutes",
        "avg_speed_mph",
        "avg_pace_mile",
        "Average.Speed",
        "Max.Speed",
        "Elevation.Gain",
        "Elevation.Loss",
        "Average.Heart.Rate",
        "Max.Heart.Rate",
        "Relative.Effort",
        "Training.Load",
        "Average.Watts",
        "Weighted.Average.Power",
        "Calories"
      )
    ),
    selectInput(
      ns("group_var"),
      "Grouping Variable",
      selected = "month",
      choices = c(
        "month",
        "year"
      )
    )
  )
}


weeklyInputsUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      ns("weekly_plot_type"),
      "Plot Style",
      selected = "Stacked Bar",
      choices = c(
        "Stacked Bar",
        "Line"
      )
    )
  )
}

# time, distance 1, distance 2, c
riegelInputsUI <- function(id){
  ns <- NS(id)
  tagList(
    numericInput(
      ns("t1"),
      "Completed Time (minutes)",
      5
    ),
    selectizeInput(
      ns("d1"),
      "Completed Distance",
      choices = c("1 Mile", "5K", "10K", "Half-Marathon", "Marathon")
    ),
    selectizeInput(
      ns("d2"),
      "Goal Distance",
      choices = c("1 Mile", "5K", "10K", "Half-Marathon", "Marathon")
    )
  )
}
  




