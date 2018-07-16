source('../src/taxi/util/index.r')
library(geosphere)
library(tidyverse)

routes <- read('google_transit_subway_static/routes.txt') %>% 
  select(-route_text_color, -agency_id, -route_url)

#todo: align it with the stops/path/route
shapes <- read('google_transit_subway_static/shapes.txt')
names(shapes) <- gsub(names(shapes),pattern = 'shape_pt_',replacement='')

shapes_edges <- shapes %>%
  group_by(shape_id) %>% mutate(nxt.lon=lead(lon), nxt.lat = lead(lat)) %>% filter(!is.na(nxt.lat)) %>%
  rowwise() %>% mutate(distance = distm(c(lon, lat), c(nxt.lon, nxt.lat), fun = distHaversine)) %>% ungroup() %>% 
  group_by(shape_id) %>% mutate(distance_traveled = cumsum(distance)) %>% ungroup()

shapes_info <- shapes_edges %>% 
  group_by(shape_id) %>% summarize(num_points = last(sequence)+2, total_length = last(distance_traveled))

stops <- read('google_transit_subway_static/stops.txt') %>%
  filter(location_type == 1) %>% select(stop_id, stop_name, lat=stop_lat, lon=stop_lon)

stop_times <- read('google_transit_subway_static/stop_times.txt',  c=c('arrival_time', 'departure_time')) %>%
  select(-ends_with('type')) %>%
  extract(col = 'stop_id', into = 'stop_id', regex= '(.*)[NS]') %>%
  mutate_at(.vars = vars(ends_with('time')), as.time24) %>%
  mutate(wait_time = diff.time(arrival_time, departure_time)) %>%
  .[,c(1,4,5,2,3,6)]

trip_path <-  stop_times %>% group_by(trip_id) %>%
  summarize(path = paste0(stop_id,collapse = '>'), stop_count = n(),
            first_stop = first(stop_id), last_stop=last(stop_id),
            first_arrival_time=first(arrival_time), last_arrival_time = last(arrival_time)) %>%
  mutate(duration = diff.time(first_arrival_time, last_arrival_time)) %>%
  ungroup()

paths <- trip_path %>% select(path,first_stop, last_stop, stop_count) %>%
  distinct() %>% mutate(path_id = rank(path)) %>% .[,c(5,1,2,3,4)]

trip_path <- translate(trip_path, paths, path, path_id, path='path_id')

trips_edges <- stop_times %>% group_by(trip_id) %>% mutate(nxt.stop_id = lead(stop_id), nxt.arrival_time = lead(arrival_time)) %>%
  filter(!is.na(nxt.stop_id)) %>% mutate(elapsed_time = diff.time(departure_time, nxt.arrival_time)) %>% select(-nxt.arrival_time) %>%
  .[,c(1,2,7,3,4,6,8)]


trips <- read('google_transit_subway_static/trips.txt',c = 'route_id') %>%
  extract(col=trip_id, regex='-.*-(.*?)-[0-9]{2}_.*_(.*)', into=c('day_type','shape_id'), remove=F) %>%
  left_join(trip_path, by='trip_id') %>%   mutate(period = categorize.timePeriod(first_arrival_time)) %>%
  select(trip_id, route_id, shape_id, path_id, day_type, direction = direction_id,trip_headsign, period, first_arrival_time, last_arrival_time, duration)

#load.mult('taxi_cleaned') while your in the data folder directory to load it.
save.mult('taxi_cleaned', paths, routes, shapes, shapes_edges, shapes_info, stop_times, stops, trips, trips_edges)
