
#####################
##### Load plot #####
#####################
# install.packages("ggiraph")
library(ggiraph)
load_plot_fun <- function(time_window) {
 
  print(c("load window:", time_window))
  if (time_window == "Week"){
    time_window_numeric <- -7
  } else if (time_window == "Month"){
    time_window_numeric <- -30
  } else if (time_window == "3 Months"){
    time_window_numeric <- -90
  } else if (time_window == "6 Months"){
    time_window_numeric <- -182
  } else if (time_window == "1 Year"){
    time_window_numeric <- -365
  } else if (time_window == "2 Years"){
    time_window_numeric <- -730
  } else if (time_window == "All"){
    time_window_numeric <- -Inf
  }
  
  daily_dat_window <- daily_dat |> 
    mutate(window_selector = date - max(date)) |> 
    filter(window_selector > time_window_numeric)
  
  
  p <- ggplot(daily_dat_window, aes(x = date)) +
    geom_line_interactive(
      aes(
        y = acute_load,
        color = "Acute (7d)",
        group = "Acute (7d)",
      ) ,
      linewidth = 1
    ) +
    geom_line_interactive(
      aes(
        y = chronic_load,
        color = "Chronic (28d)",
        group = "Chronic (28d)",
      ),
      linewidth = 1
    ) +
    geom_point_interactive(
      aes(
        y = acute_load,
        tooltip = glue(
          "
          Date: {date}
          Acute load: {round(acute_load, 1)} mi
          Chronic load: {round(chronic_load, 1)} mi
          "
        ),
      ),
      size = 1,
      alpha = 0
    ) +
    geom_point_interactive(
      aes(
        y = chronic_load,
        tooltip = glue(
          "
          Date: {date}
          Acute load: {round(acute_load, 1)} mi
          Chronic load: {round(chronic_load, 1)} mi
          "
        ),
      ),
      size = 1,
      alpha = 0
    ) +
    labs(
      x = "Date",
      y = "Miles",
      color = "Load",
      title = "Training Load Over Time"
    ) +

    scale_color_manual(values = running_palette) +
    theme_minimal()+
  theme(legend.position = "bottom")
  
  girafe(
    ggobj = p,
    width_svg = 10,
    height_svg = 5,
    options = list(
      opts_hover(css = "stroke-width:3;opacity:1;"),
      opts_hover_inv(css = "opacity:0.15;"),
      opts_tooltip(
        css = "
        background-color: #141822;
        color: #E6E6E6;
        border-radius: 10px;
        padding: 10px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.4);
      "
      ),
      opts_zoom(max = 6),
      opts_selection(type = "single", css = "stroke-width:4;"),
      opts_toolbar(saveaspng = TRUE, position = "topright")
    )
  )

}

########################
##### Chronic load #####
########################

# Need to look at what this means a bit more
# Jan 24 2026: this does not seem useful, will not add into app but will leave
# the function in case things change
ratio_plot_fun <- function() {
  ggplot(daily_dat, aes(x = date, y = load_ratio)) +
    geom_line() +
    geom_hline(yintercept = c(0.8, 1.3, 1.5), linetype = "dashed", alpha = 0.5) +
    theme_bw() +
    labs(
      title = "Acute : Chronic Load Ratio",
      y = "Load Ratio",
      x = "Date"
    )
}

#########################
##### Fitness Trend #####
#########################


# efforts$fitness_hat <- predict(fitness_model)





##### Load fitness #####

# ok jack so this plot seems like it could be useful, but i need to dig more into
# the readiness score to figure out what it is
# readiness score is based on 3 things:
# chronic_load: 28 day average mileage
# load_ratio: ratio of acute to chronic: 7 / 28 load 
  # 1.3–1.5 → elevated injury & overreach risk
  # ~1.0 → stable training
  # <0.8 → detraining / taper
# monotony: repetitive nature of training: higher monotony is worse
#

# What this plot is showing is "in what training regimes does high acute load
# coexist with high readiness?"
# It is interesting that the 2 big years of consistent training (2024 & 2025) both
# follow the same U-shaped pattern. My interpretation of this at the moment is
# that the readiness metric is conditional on training phase. Since readiness score
# is high when i have either lower or higher acute load, during lower acute load,
# this is probably during a taper or shortly after a big build and race. Readiness
# would be higher at these times. When readiness is high and so is acute load, 
# these times are likely during peak weeks of the build, when my fitness is high
# which is only something that affects the readiness, not the acute load (load is
# currently just a metric of mileage, not of intensity within miles or actual 
# load from a specific run ie it treats a 7 mile interval run the same as a 7 
# mile easy run). I think I may need to do some more analysis to confirm this
# hypothesis or perhaps tweak some of my calculations such as by putting 
# coefficients in front of the readiness score values to more closely model 
# my empirical evidence
load_fitness_fun <- function() {
  ggplot(
    daily_dat |> filter(!is.na(acute_load)),
    aes(x = acute_load, y = readiness, color = as.factor(year))
  ) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "gam", se = FALSE) +
    theme_bw() +
    labs(
      title = "Training Load vs Readiness",
      x = "Acute Load",
      y = "Readiness Score",
      color = "Year"
    )+
    scale_color_manual(values = running_palette)
}


