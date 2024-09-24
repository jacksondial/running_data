# Makes landing page objects

total_miles_vb <-  shinydashboard::valueBox(
  value = sum(app_dat$Distance),
  subtitle = "Total Miles Ran",
  icon = icon("running"),
  width = 4,
  # color = "navy"
  # style = "background-color: #33b6ff; color: white;"
)



# red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.



miles_24_vb <- shinydashboard::valueBox(
  value = app_dat |> filter(year == "2024") |> summarize(total_distance = sum(Distance)) |> pull(total_distance),
  subtitle = "Miles Ran in 2024",
  icon = icon("running"),
  width = 4
)