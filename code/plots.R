# Plotting functions
# x_var <- "Elapsed.Time"

##### Total Barplot #####

barplot_fun <- function(x_var){
  ggplot(app_dat, aes(x = !!sym(x_var)))+
    geom_histogram(aes(fill = running_palette[4]))+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          legend.position = "none")+
    scale_fill_manual(values = running_palette[4])
}

# corr_x_var <- "Elapsed.Time"
# corr_y_var <- "Distance"
# group_var <- "month"


##### Correlation Plot #####

corr_plot <- function(corr_x_var, corr_y_var, group_var){
  ggplot(app_dat)+
    geom_point(aes(x = !!sym(corr_x_var), 
                   y = !!sym(corr_y_var),
                   color = as.factor(!!sym(group_var))),
               size = 2.5,
               alpha = .6)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom")+
    scale_color_brewer(palette = "Paired")+
    labs(color = "")
  
}

##### Weekly plot #####

# this_week <- activity_dat2 |> filter(year == 2025, week_monday == 42)
weekly_bar_fun <- function(weekly_plot_type){
  print(c("here jack", weekly_plot_type))
  weekly_dat <- app_dat |> 
    group_by(year, week_monday) |> 
    summarise(weekly_mileage = sum(Distance)) |> 
    mutate(above_40_m = ifelse(weekly_mileage > 40, T, F))
  
  if (weekly_plot_type == "Stacked Bar"){
    ggplot(weekly_dat, aes(x = week_monday, y = weekly_mileage))+
      geom_col(aes(fill = as.factor(year)))+
      theme_bw()+
      scale_fill_manual(values = running_palette)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
      )+
      labs(
        x = "Week of Year (Monday-Sunday)",
        y = "Weekly Mileage",
        fill = "Year")
    
  } else if (weekly_plot_type == "Line"){
    ggplot(weekly_dat, aes(x = week_monday, y = weekly_mileage))+
      geom_line(aes(color = as.factor(year)))+
      geom_point(aes(color = as.factor(year)))+
      theme_bw()+
      scale_color_manual(values = running_palette)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
      )+
      labs(
        x = "Week of Year (Monday-Sunday)",
        y = "Weekly Mileage",
        color = "Year")
  }
  
}


#####################
##### Load plot #####
#####################

load_plot_fun <- function() {
  ggplot(daily_dat, aes(x = date)) +
    geom_line(aes(y = acute_load, color = "Acute (7d)"), linewidth = 1) +
    geom_line(aes(y = chronic_load, color = "Chronic (28d)"), linewidth = 1) +
    theme_bw() +
    labs(
      x = "Date",
      y = "Miles",
      color = "Load",
      title = "Training Load Over Time"
    )+
    scale_color_manual(values = c("lightpink", "dodgerblue"))
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
fitness_model <- mgcv::gam(best_30d_speed ~ s(as.numeric(date)), data = efforts)

# efforts$fitness_hat <- predict(fitness_model)

fitness_plot_fun <- function() {
  ggplot(efforts, aes(x = date)) +
    geom_point(aes(y = best_30d_speed), alpha = 0.3) +
    geom_line(aes(y = fitness_hat), color = "blue", linewidth = 1) +
    theme_bw() +
    labs(
      title = "Estimated Fitness Trend",
      y = "Best 30-day Speed (mph)",
      x = "Date"
    )
}



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




