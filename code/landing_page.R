# Makes landing page objects
source("init.R")
# total_miles_vb <-  shinydashboard::valueBox(
#   value = sum(app_dat$Distance),
#   subtitle = "Total Miles Ran",
#   icon = icon("running"),
#   width = 4,
#   
# )

total_miles_vb <- bslib::value_box(
  title = "Lifetime Miles Ran",
  value = round(sum(app_dat$distance_miles),2),
  showcase = icon("running"), # Customize icon style
  theme = value_box_theme(bg = running_palette[1], fg = running_palette[2]), # Sets a green theme color
  fill = TRUE,
  height = 200L
)

miles_24_vb <- bslib::value_box(
  title = "Miles Ran in 2024",
  value = app_dat |> filter(year == "2024", Activity.Type == "Run") |> summarize(total_distance = round(sum(distance_miles), 2)) |> pull(total_distance),
  showcase = icon("person-running"),
  theme = value_box_theme(bg = running_palette[2], fg = running_palette[1]),
  fill = TRUE, 
  height = 200L
)


# miles_24_vb <- shinydashboard::valueBox(
#   value = app_dat |> filter(year == "2024") |> summarize(total_distance = sum(Distance)) |> dplyr::pull(total_distance),
#   subtitle = "Miles Ran in 2024",
#   icon = icon("running"),
#   width = 4
# )