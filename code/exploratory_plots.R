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
    scale_color_manual(values = running_palette)+
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



