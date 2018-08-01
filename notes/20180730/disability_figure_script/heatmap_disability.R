source('methods.R')
source('../../../src/path_finding.R')
library(igraph)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(mapview)
library(sp)
load('data.rdata')

#grid of points besides staten island
#15615 points
grid <- create_grid(lat = stops$stop_lat, lon = stops$stop_lon, neighborhood = nyc_neighborhoods, borough_blacklist = 'Staten Island')

#graph with mean deduplicate the weird cases
graph <- igraph_edges %>% mutate(weight = mean,
                                 stop_id = ifelse(nchar(stop_id)==4, substr(stop_id,1,3), stop_id),
                                 nxt_stop_id = ifelse(nchar(nxt_stop_id == 4), substr(nxt_stop_id,1,3), nxt_stop_id)) %>% 
  group_by(stop_id, nxt_stop_id) %>% summarize(weight = mean(weight)) %>%
  graph.data.frame()

#distance matrix from every stop to every other stop
distance_matrix <- distances(graph)

#point and their closest stop
#the stops that are excluded are typically rare stops that were filtered out
stops_undirected <- stops %>% filter(nchar(stop_id) == 3 & !stop_id %in% c("H19",'N12','S12','S10') )

point2station_matrix <- merge(grid, stops_undirected, by = NULL) %>% 
  mutate(distance = Distance(lat, lon, stop_lat, stop_lon)) %>% arrange(distance)

#speed min per km
#walking avg = 5 km per hour
walking_speed <- 12

#wheel avg = 3.9 km per hour
wheel_speed <- 15.4
#dest
dest <- 'A31'

point2station_walking <- compute_point2station(point2station_matrix, speed = walking_speed) %>%
  mutate(total_time_bin = floor(total_time/5)) %>% arrange(total_time)


point2station_wheel <- compute_point2station(point2station_matrix, speed = wheel_speed,
                                             stop_whitelist = accessible_stations) %>%
  mutate(total_time_bin = floor(total_time/5)) %>% arrange(total_time)

#plot total commute time difference
diff_time <- point2station_wheel %>% left_join(point2station_walking, by = c('lat', 'lon', 'neighborhood', 'borough')) %>% 
  mutate(diff = total_time.x -total_time.y) 

point2station_diff <- diff_time %>% select(lon, lat,neighborhood, borough,total_time = diff) %>% 
  mutate(total_time_bin = floor(total_time/5)) %>% arrange(total_time)

diff_time %>% ggplot(aes(x = diff)) + geom_histogram()
#diff_time summary
summary(diff_time$diff)

#plot distribution commute time
rbind(mutate(point2station_wheel, type ="wheel"), mutate(point2station_walking, type = "walking")) %>%
  ggplot(aes(x=total_time, fill = factor(type))) + geom_density() + facet_wrap(facets = ~type)

#diff by borough
diff_time %>% ggplot(aes(x= diff, fill = borough)) + geom_density() + facet_grid(facets = ~borough)

worst_5_neighborhood <- diff_time %>% group_by(neighborhood) %>% summarize( m = mean(diff)) %>% arrange(desc(m)) %>% mutate(order =1:nrow(.)) %>%
  filter(order <= 5) %>% pull(neighborhood)

#top 5 worst neighborhood
diff_time %>% filter(neighborhood %in% worst_5_neighborhood) %>%
  ggplot(aes(x= diff, fill = neighborhood)) + geom_density() + facet_grid(facets = ~neighborhood)


pal <- create_pal(0,150)
pal_bin <- create_pal(0,30)
pal_diff <- create_pal(0,70)
pal_diff_bin <- create_pal(0,14)


map_wheel <- create_map(point2station_wheel, pal, dest = dest, stops = stops)
map_walking <- create_map(point2station_walking, pal, dest = dest, stops = stops)
map_diff <- create_map(point2station_diff, pal_diff, dest = dest, stops = stops, title = "Difference in commute time to")

save(file = 'post_data.rdata', list = ls())
