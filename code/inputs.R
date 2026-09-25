
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
  





# COROS section. The race date drives the taper countdown; COROS has no race
# date of its own (queryTrainingSchedule returns nothing), so it is entered here.
corosInputsUI <- function(id){
  ns <- NS(id)
  tagList(
    dateInput(
      ns("race_date"),
      "Race Date",
      value = Sys.Date() + 14,
      min = Sys.Date() - 365,
      format = "M d, yyyy"
    ),
    textInput(
      ns("goal_time"),
      "Goal Time (h:mm:ss)",
      value = "2:45:00",
      placeholder = "2:45:00"
    ),
    helpText(
      class = "text-muted",
      "COROS snapshot is a point-in-time export; see the Data & Refresh tab."
    )
  )
}
