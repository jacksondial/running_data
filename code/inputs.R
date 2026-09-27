# Input modules for the app.

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

# Shared timeframe control for the COROS charts. Rendered once per chart tab
# (Shiny needs unique ids) and kept in sync by observers in server.R, so the
# choice follows you from tab to tab.
COROS_RANGES <- c(
  "Last 30 days",
  "Last 3 months",
  "Last 6 months",
  "Last 12 months",
  "All time"
)

corosRangeUI <- function(id, selected = "Last 3 months") {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("range"),
      "Timeframe",
      choices = COROS_RANGES,
      selected = selected
    ),
    div(class = "coros-range-note", uiOutput(ns("range_note")))
  )
}
