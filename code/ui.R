# UI for app
#
# Assembles the navbar: a shared landing page, then one section per data source.
# Strava analyses live in ui_strava.R; the COROS placeholder in ui_coros.R.
# App-wide CSS lives in ui_styles.R.
source("inputs.R")
source("landing_page.R")
source("init.R", local = TRUE)
source("ui_styles.R")
source("ui_strava.R")
source("ui_coros.R")


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

  # Shared landing page, then one nav_menu per data source.
  tabPanel(
    "Landing Page",
    value = "landing_page",
    card(
      div(
        class = "lp-hero",
        h2("Running Snapshot"),
        p("Quick look at training progression, annual volume, and current readiness. Derived from the Strava export.")
      ),
      layout_column_wrap(
        class = "lp-grid",
        width = 1 / 3,
        gap = "1rem",
        lifetime_miles_vb,
        miles_this_year_vb,
        miles_last_year_vb,
        avg_weekly_this_year_vb,
        longest_run_this_year_vb,
        readiness_box
      )
    )
  ),
  strava_nav(),
  coros_nav()
)
