
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

load_fitness_fun <- function() {
  ggplot(
    daily_dat |> filter(!is.na(acute_load)),
    aes(x = acute_load, y = readiness, color = as.factor(year))
  ) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "gam") +
    theme_bw() +
    labs(
      title = "Training Load vs Readiness",
      x = "Acute Load",
      y = "Readiness Score",
      color = "Year"
    )+
    scale_color_manual(values = running_palette)
}


