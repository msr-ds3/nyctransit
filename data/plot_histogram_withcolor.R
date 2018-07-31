plot_histogram <- function(all_itin_df, start_hour = 7 , end_hour = 10, bins = 60, route_color_dict) { 
  
  plot_data <- all_itin_df %>%
    mutate(day_of_week = weekdays(leg1_start_time),
           hour = hour(leg1_start_time)) %>%
    filter(isWeekend(day_of_week) == F,
           hour >= start_hour & hour < end_hour)
  
  plot_data$itin_id <- as.factor(plot_data$itin_id)
  
  colors <- route_color_dict$route_color
  names(colors) <- route_color_dict$route_short_name
  
  plot_lines <- substr(unique(plot_data$label), 1, 1)
  plot_colors <- as.vector(colors[plot_lines])
  
  plot <- plot_data %>%
    ggplot(aes(x = as.numeric(time_diff), group=itin_label, fill=itin_label)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = bins) +
    xlab('Trip Time (in minutes)') +
    ylab('Number of Trips') +
    theme(legend.position = "bottom", legend.direction = "vertical", legend.key.size = unit(1.5, 'lines')) +
    #xlim(as.numeric(quantile(mj_df$time_diff, c(0.1, 0.9)))) +
    scale_fill_manual(guide = guide_legend(), values = plot_colors) + 
    scale_color_manual(values = plot_colors)
  
  # geom_histogram(), position = "identity", alpha = 0.5)
  
  return(plot)
}