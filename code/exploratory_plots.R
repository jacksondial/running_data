# Plotting functions
# x_var <- "Elapsed.Time"

# corr_x_var <- "Elapsed.Time"
# corr_y_var <- "Distance"
# group_var <- "month"


##### Correlation Plot #####

corr_plot <- function(corr_x_var, corr_y_var, group_var){
  corr_dat <- app_dat |>
    dplyr::filter(Activity.Type == "Run")

  group_values <- corr_dat[[group_var]]
  n_groups <- dplyr::n_distinct(group_values, na.rm = TRUE)

  ggplot(corr_dat)+
    geom_point(aes(x = !!sym(corr_x_var), 
                   y = !!sym(corr_y_var),
                   color = as.factor(!!sym(group_var))),
               size = 2.5,
               alpha = .6)+
    theme_running_dark() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = palette_categorical(n_groups))+
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
      theme_running_dark() +
      scale_fill_manual(values = palette_categorical(dplyr::n_distinct(weekly_dat$year)))+
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
      theme_running_dark() +
      scale_color_manual(values = palette_categorical(dplyr::n_distinct(weekly_dat$year)))+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
      )+
      labs(
        x = "Week of Year (Monday-Sunday)",
        y = "Weekly Mileage",
        color = "Year")
  }
  
}


