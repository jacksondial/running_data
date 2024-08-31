# Plotting functions


barplot_fun <- function(x_var){
  ggplot(activity_dat2, aes(x = !!sym(x_var)))+
    geom_histogram()+
    theme(panel.grid.minor = element_blank())
}


