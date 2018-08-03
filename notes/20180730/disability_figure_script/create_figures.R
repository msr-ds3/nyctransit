#run create_figures_data.r before running this
library(tidyverse)
library(leaflet)
load('figure_data.rdata')

#palettes
pal <- create_pal(0,180)
pal_bin <- create_pal(0,36)
pal_diff <- create_pal(0,70)
pal_diff_bin <- create_pal(0,14)

plot_dist_difference <- diff_time %>% ggplot(aes(x = diff)) + geom_histogram()
ggsave(filename = 'figures/Difference_Commute_time_distribution.png', plot_dist_difference)
#diff_time summary
summary(diff_time$diff)

#plot distribution commute time
plot_dist_commute_time <- rbind(mutate(point2station_wheel, type ="handicap"), mutate(point2station_walking, type = "normal")) %>%
  ggplot(aes(x=total_time, fill = type)) + geom_density() + facet_wrap(facets = ~type)
ggsave(filename = 'figures/Commute_time_distribution(normal_vs_handicap).png', plot_dist_commute_time)

#diff by borough
plot_dist_commute_time_by_borough <- diff_time %>% ggplot(aes(x= diff, fill = borough)) + geom_density() + facet_grid(facets = ~borough)
ggsave(filename = 'figures/Commute_time_distribution_by_borough(handicap).png', plot_dist_commute_time_by_borough)

worst_5_neighborhood <- diff_time_neighborhood %>%
  filter(order <= 5) %>% pull(neighborhood)

#top 5 worst neighborhood
plot_dist_difference_worst_neighborhood <- diff_time %>% filter(neighborhood %in% worst_5_neighborhood) %>%
  ggplot(aes(x= diff, fill = neighborhood)) + geom_density() + facet_grid(facets = ~neighborhood)
ggsave(filename = 'figures/Difference_Commute_time_distribution_worst_neighborhoods.png', plot_dist_difference_worst_neighborhood)

stops_exclude_si <- stops_undirected %>% filter(!startsWith(stop_id, prefix = "S"))
stops_exclude_si_wheel <- stops_exclude_si %>% filter(stop_id %in% accessible_stations)
stops_exclude_si_walking <- stops_exclude_si %>% filter(!stop_id %in% accessible_stations)

map_wheel <- create_map(point2station_wheel, pal, dest = dest, stops = stops) %>% 
  addCircles(lat =stops_exclude_si_wheel$stop_lat, lng = stops_exclude_si_wheel$stop_lon, radius = 100, opacity = 1, fillOpacity = 1)
mapshot(map_wheel, file = 'heatmap_handicap.png')

map_walking <- create_map(point2station_walking, pal, dest = dest, stops = stops, color_source = point2station_wheel)%>%
  # addCircles(lat = stops_exclude_si$stop_lat, lng = stops_exclude_si$stop_lon)
  addCircles(lat =stops_exclude_si_walking$stop_lat, lng = stops_exclude_si_walking$stop_lon) %>%
  addCircles(lat = stops_exclude_si_wheel$stop_lat, lng = stops_exclude_si_wheel$stop_lon, radius = 100, opacity = 1, fillOpacity = 1)
mapshot(map_walking, file = 'heatmap_normal.png')

map_diff <- create_map(point2station_diff, pal_diff, dest = dest, stops = stops, title = "Difference in commute time <br>to") %>%
  addCircles(lat =stops_exclude_si_wheel$stop_lat, lng = stops_exclude_si_wheel$stop_lon, radius = 100, opacity = 1, fillOpacity = 1) %>%
  addCircles(lat =stops_exclude_si_walking$stop_lat, lng = stops_exclude_si_walking$stop_lon) %>%
  addPolygons( data = nyc_neighborhoods[nyc_neighborhoods$neighborhood %in% worst_5_neighborhood,], popup = ~neighborhood) 
mapshot(map_diff, file ='heatmap_difference(normal_vs_handicap).png')

point2station_wheel_f <- point2station_wheel %>% filter(travel_time <= 20)
point2station_walking_f <- point2station_walking %>% filter(travel_time <= 20)                   
map_wheel_f <- create_map(point2station_wheel_f, pal, dest = dest, stops = stops)
map_walking_f <- create_map(point2station_walking_f, pal, dest= dest, stops = stops)

nrow(point2station_wheel_f) / nrow(point2station_walking_f)
leaflet(nyc_neighborhoods) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.89,  40.73, zoom = 13, options = c('zoomSnap:.25')) %>%
  addCircleMarkers(lng = point2station_walking_f$lon, lat = point2station_walking_f$lat, opacity = 1, fillOpacity = 1,
                   color ='red') %>% 
  addCircleMarkers(lng = point2station_wheel_f$lon, lat = point2station_wheel_f$lat, opacity = .5, fillOpacity = 1,
                   color ='green') %>% 
  htmlwidgets::onRender(
    'function(el, x){
    overlay = el.querySelector(".leaflet-overlay-pane")
    overlay.style.opacity = ".6"}'
  ) %>% mapshot(file = 'difference_in_coverage.png', vwidth = 2000, vheight = 3000)
sync(map_wheel_f, map_walking_f)
