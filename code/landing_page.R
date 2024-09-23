# Makes landing page objects

total_miles_vb <- shinydashboard::valueBox(
  value = tags$div(
    class = "value-icon",
    tags$h3(sum(app_dat$Distance)),
    icon("running")
  ),
  subtitle = tags$p("Total Miles Ran"),
  width = 4
)