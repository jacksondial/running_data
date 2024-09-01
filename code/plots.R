# Plotting functions
# x_var <- "Elapsed.Time"

barplot_fun <- function(x_var){
  ggplot(activity_dat2, aes(x = !!sym(x_var)))+
    geom_histogram(aes(fill = "orange"))+
    theme(panel.grid.minor = element_blank(),
          legend.position = "none")+
    scale_fill_manual(values = "orange2")
}

# corr_x_var <- "Elapsed.Time"
# corr_y_var <- "Distance"
# group_var <- "month"

corr_plot <- function(corr_x_var, corr_y_var){
  ggplot(activity_dat2)+
    geom_point(aes(x = !!sym(corr_x_var), 
                   y = !!sym(corr_y_var),
                   color = as.factor(!!sym(group_var))),
               size = 2.5,
               alpha = .6)+
    theme(panel.grid.minor = element_blank())+
    scale_color_brewer(palette = "Paired")
  
}

