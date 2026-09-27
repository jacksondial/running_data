# UI for the app: the navbar page. Tabs are built in ui_coros.R, CSS lives in
# ui_styles.R, and shared setup lives in init.R.

source("init.R")

ui <- page_navbar(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    bg = "#1E242C",
    fg = "#E6EEF5",
    primary = "#00C2FF",
    secondary = "#2A313B",
    success = "#23D18B",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    font_scale = 1.1
  ),
  title = "Marathon Intelligence Platform",
  id = "title",
  header = app_styles(),
  landing_tab(),
  !!!coros_tabs()
)
