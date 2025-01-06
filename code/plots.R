# Plotting functions
# x_var <- "Elapsed.Time"

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

