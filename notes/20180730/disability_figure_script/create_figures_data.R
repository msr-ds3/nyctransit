#run load_data.r first
source('methods.R')
source('../../../src/path_finding.R')
library(igraph)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(mapview)
library(sp)
load('data.rdata')
load('../../../data/wait_times.RData')
#grid of points besides staten island
#15615 points
grid <- create_grid(lat = stops$stop_lat, lon = stops$stop_lon, neighborhood = nyc_neighborhoods, borough_blacklist = 'Staten Island',
                    neighborhood_blacklist = c(
                      'Broad Channel', 'Jamaica Bay', 'Ellis Island', 'Liberty Island', 'Governors Island',
                      'Hart Island', 'City Island', 'Rikers Island', 'South Brother Island', 'North Brother Island',
                      'Roosevelt Island'))

#graph with mean deduplicate the weird cases
graph <- igraph_edges %>% mutate(weight = `90%`,
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
dest <- '635'

wait_time <- wait_times %>% 
  filter(day %in% 1:5 & time_of_day == 'Early morning') %>% 
  mutate(stop_id = substr(stop_mta_id, 1,3)) %>% group_by(stop_id) %>% summarize(weight = mean(pred_90th_wait)) 

point2station_walking <- compute_point2station(point2station_matrix, speed = walking_speed, wait_time = wait_time) %>%
  mutate(total_time_bin = floor(total_time/5)) %>% arrange(total_time)


point2station_wheel <- compute_point2station(point2station_matrix, speed = wheel_speed,
                                             stop_whitelist = accessible_stations, wait_time = wait_time) %>%
  mutate(total_time_bin = floor(total_time/5)) %>% arrange(total_time)

#plot total commute time difference
diff_time <- point2station_wheel %>% left_join(point2station_walking, by = c('lat', 'lon', 'neighborhood', 'borough')) %>% 
  mutate(diff = total_time.x -total_time.y) 

point2station_diff <- diff_time %>% select(lon, lat,neighborhood, borough,total_time = diff) %>% 
  mutate(total_time_bin = floor(total_time/5)) %>% arrange(total_time)

diff_time_neighborhood <- diff_time %>% 
  group_by(neighborhood) %>% 
  summarize(diff = mean(diff)) %>% arrange(desc(diff)) %>% 
  mutate(order =1:nrow(.))





save(file = 'figure_data.rdata', list = ls())
