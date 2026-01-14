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
miles_25_vb <- bslib::value_box(
  title = "Miles Ran in 2025",
  value = app_dat |> filter(year == "2025", Activity.Type == "Run") |> summarize(total_distance = round(sum(distance_miles), 2)) |> pull(total_distance),
  showcase = icon("person-running"),
  theme = value_box_theme(bg = running_palette[1], fg = running_palette[4]),
  fill = TRUE, 
  height = 200L
)

miles_24_vb <- bslib::value_box(
  title = "Miles Ran in 2024",
  value = app_dat |> filter(year == "2024", Activity.Type == "Run") |> summarize(total_distance = round(sum(distance_miles), 2)) |> pull(total_distance),
  showcase = icon("person-running"),
  theme = value_box_theme(bg = running_palette[1], fg = running_palette[5]),
  fill = TRUE, 
  height = 200L
)

# "#002147" "#A87C55" "#FFD700" "#8B1D1D" "#004E64" "#F4EDE4" "#3E2C1C" "#D4AF37" "#3B5A52" "#C65353"

# miles_24_vb <- shinydashboard::valueBox(
#   value = app_dat |> filter(year == "2024") |> summarize(total_distance = sum(Distance)) |> dplyr::pull(total_distance),
#   subtitle = "Miles Ran in 2024",
#   icon = icon("running"),
#   width = 4
# )

latest <- daily_dat |> filter(date == max(date))

readiness_box <- bslib::value_box(
  title = "Readiness",
  value = latest$insight,
  showcase = icon("heartbeat"),
  theme = value_box_theme(bg = running_palette[1], fg = running_palette[6]),
  fill = TRUE,
  height = 200L
)

# readiness_box <- renderUI({
#   valueBox(
#     value = round(latest$readiness, 2),
#     subtitle = latest$insight,
#     icon = icon("heartbeat"),
#     color = ifelse(latest$readiness > 1, "green", "orange")
#   )
# })


