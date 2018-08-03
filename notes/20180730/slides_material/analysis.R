library(leaflet)
library(mapview)
source('../../../src/time.R')
stop_times <- read_csv('../../../data/taxi_cleaned/stop_times.csv', col_types = cols(stop_id.u = col_character()))
stops <- read_csv('../../../data/taxi_cleaned/stops.csv')
trips <- read_csv('../../../data/taxi_cleaned/trips.csv', col_types = cols(duration = col_double(), route_id = col_character())) %>%
  mutate(time_period = categorize.timePeriod(first_arrival_time)) 
routes <- read_csv('../../../data/google_transit_subway_static/routes.txt') %>% mutate(route_color = ifelse(is.na(route_color), '#000000',paste0('#',route_color)))

a_trips <- trips %>% filter(route_id == 'A') %>% group_by(path_id) %>% summarize(trip_id = first(trip_id))

stop_times_loc <- left_join(stop_times, stops)

test <- stop_times_loc %>% group_by(stop_id, stop_lat, stop_lon) %>% summarize(c = n()) %>% 
  mutate(direction = substring(stop_id,4))


unique_trips <- trips %>% group_by(path_id, route_id) %>% summarize(trip_id = first(trip_id))
  
unique_trips <- left_join(unique_trips,stop_times_loc) %>%left_join(routes) %>% 
  mutate(direction = substring(stop_id,4))

a_map <-leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.89,  40.76, zoom = 12, options = c('zoomSnap:.25'))
  
i <- 1
for (trip_id in unique(unique_trips$trip_id)){
  print(i)
  i <- i+1
  t <- unique_trips[unique_trips$trip_id == trip_id,] %>%
    mutate(stop_lon = ifelse(direction == 'N', stop_lon + .01, stop_lon),
           opacity = ifelse(direction == 'N', .5, 1))
  a_map <- a_map %>%
    addPolylines(lng = t$stop_lon, lat = t$stop_lat, color = t$route_color, weight = 2, opacity = .1) %>%
    addCircles(lng = t$stop_lon, lat = t$stop_lat, color = t$route_color,fillOpacity = .1) 
}
mapshot(a_map, file = 'train_map_direction.png', vheight = 1500)

a_map <-leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.89,  40.76, zoom = 12, options = c('zoomSnap:.25'))

i <- 1
for (trip_id in unique(unique_trips$trip_id)){
  print(i)
  i <- i+1
  t <- unique_trips[unique_trips$trip_id == trip_id,]
  a_map <- a_map %>%
    addPolylines(lng = t$stop_lon, lat = t$stop_lat, color = t$route_color, weight = 2, opacity = .1) %>%
    addCircles(lng = t$stop_lon, lat = t$stop_lat, color = t$route_color,fillOpacity = .1) 
}
mapshot(a_map, file = 'train_map_normal.png', vheight = 1500)