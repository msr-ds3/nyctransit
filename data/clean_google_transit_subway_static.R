library(tidyverse)
library(assertthat)
library(geosphere)
source('../src/read.R')
source('../src/translate.R')
source('../src/time.R')

#================================#
#Load raw data(columns with all nas are removed automatically)
#================================#
routes <- read('google_transit_subway_static/routes.txt')
shapes <- read('google_transit_subway_static/shapes.txt')
stops <- read('google_transit_subway_static/stops.txt')

#reads arrival_time and departure_time as character(to prevent parsing error when hour is greater than 24)
stop_times <- read('google_transit_subway_static/stop_times.txt',  c=c('arrival_time', 'departure_time'))
#reads route_id as character(prevents parsing error since read_csv will think route_id is an int based on the first 1k line)
trips <- read('google_transit_subway_static/trips.txt',c = 'route_id')
transfer <- read('google_transit_subway_static/transfers.txt')
stations <- read('http://web.mta.info/developers/data/nyct/subway/Stations.csv')

#================================#
# Preprocess data
#================================#
#routes: removes junk columns
routes <- routes %>% select(-route_text_color, -agency_id, -route_url)

#shapes: renames columns to remove the shape_pt_ prefix. e.g from shape_pt_lat to lat
names(shapes) <- gsub(names(shapes),pattern = 'shape_pt_',replacement='')

#stop_times: remove pickup_type and drop_off_type column
stop_times <- stop_times %>% select(-ends_with('type'))

#stop_times: convert time into seconds from midnight
stop_times <- stop_times %>% mutate_at(.vars = vars(ends_with('time')), as.time24)

#stop_times: extract the undirected stop_id from stop_id column. e.g. from 101N to 101
stop_times <- stop_times %>% extract(col = 'stop_id', into = 'stop_id.u', regex= '(.*)[NS]', remove = F)

#stop_times: compute wait_time (this is the difference between arrival_time and departure_time)
stop_times <- stop_times %>% mutate(wait_time = diff.time(arrival_time, departure_time))

#trips: extracts day_type and shape_id from the trips table. The some entries in the original shape_id column contains na values
trips <- trips %>% extract(col=trip_id, regex='-.*-(.*?)-[0-9]{2}_.*_(.*)', into=c('day_type','shape_id'), remove=F)

#stations: rename GTFS_Stop_ID to stop_id, and Complex_ID to complex_id
stations <-stations %>% rename(stop_id = GTFS_Stop_ID, complex_id = Complex_ID)

#================================#
# shape_edges and shape_info
#================================#

#shape_edges: create a list of edges
shape_edges <- shapes %>% group_by(shape_id) %>% mutate(nxt.lon=lead(lon), nxt.lat = lead(lat)) %>% filter(!is.na(nxt.lat)) 

#shape_edges: compute the distance between edges using distm
shape_edges <- shape_edges %>% rowwise() %>% mutate(distance = distm(c(lon, lat), c(nxt.lon, nxt.lat), fun = distHaversine)) %>% ungroup()

#shape_edges: compute the cumsum of distance
shape_edges <- shape_edges %>% group_by(shape_id) %>% mutate(distance_traveled = cumsum(distance)) %>% ungroup()

#shapes_info: summarizes each shape. records how many shape_pt the shape has, and how long it is
shapes_info <- shape_edges %>% group_by(shape_id) %>% summarize(num_points = last(sequence)+2, total_length = last(distance_traveled))

#shape_info: adds route_id to each shape
shapes_info <- shapes_info %>% translate(trips, shape_id, route_id,shape_id = route_id)

#================================#
# trips_edges
#================================#
#trip_edges: create edges, filters out the incomplete edges(at the end of a trip)
trip_edges <- stop_times %>% group_by(trip_id) %>% mutate(nxt.stop_id = lead(stop_id),nxt.stop_id.u = lead(stop_id.u), nxt.arrival_time = lead(arrival_time)) %>%
  filter(!is.na(nxt.stop_id)) %>% ungroup
#trip_edges: adds time_diff column which is the time from when the train arrives at the current stop to when the train arrives at the next stop
#trip_edges: adds time_diff_d column which is the time from when the train leaves the current stop to when it arrives at the next stop
trip_edges <- trip_edges %>% mutate(time_diff = diff.time(arrival_time, nxt.arrival_time), time_diff_d = diff.time(departure_time, nxt.arrival_time))
#================================#
# path
#================================#
#path_trips: the path that each trips takes as well as the first/last stop and the arrival times
path_trips <-  stop_times %>% group_by(trip_id) %>%
  summarize(path = paste0(stop_id,collapse = '>'), stop_count = n(),
            first_stop = first(stop_id), last_stop=last(stop_id),
            first_arrival_time=first(arrival_time), last_arrival_time = last(arrival_time)) %>% ungroup()

#path_trips:calculate the duration of the trip
path_trips <- path_trips %>% mutate(duration = diff.time(first_arrival_time, last_arrival_time))

#path_trips: add direction
path_trips <- path_trips %>% translate(trips, trip_id, direction_id, trip_id = direction_id)

#path_trips: add route_id
path_trips <- path_trips %>% translate(trips, trip_id, route_id, trip_id = route_id)

#path_popularity: how often a given path occurs
path_popularity <- path_trips %>% group_by(path, route_id) %>% summarize(popularity = n()) 

#path_popularity: adds relative popularity.
path_popularity <- path_popularity %>% group_by(route_id) %>% mutate(relative_popularity = popularity/sum(popularity))



#paths: contains all possible path, the first_stop, last_stop, the route_id 
paths <- path_trips %>% select(path,first_stop, last_stop, stop_count, route_id,direction_id) %>% distinct

#paths: create unique path_id
paths <- paths %>% group_by(route_id) %>% mutate(path_id = paste(route_id,rank(path),sep='_')) 

#paths: join with popularity
paths <- paths %>% translate(path_popularity, path, c(popularity, relative_popularity), path=c(popularity, relative_popularity))

#================================#
# trips
#================================#
#trips: add path_id
trips <- trips %>% translate(path_trips, trip_id, 
                             c(path,first_stop, last_stop, first_arrival_time, last_arrival_time, duration),
                             trip_id=c(path,first_stop, last_stop, first_arrival_time, last_arrival_time, duration)) %>% translate(paths, path, path_id, path=path_id)
#trips: add 
#================================#
# sanity checks
#================================#

#check there is a many to one relationship between  shape_id and route_id.( A given shape_id should only have one route_id associated with it)
many_to_one_shape_route <-trips %>% select(route_id, shape_id) %>% distinct %>% group_by(shape_id) %>% summarize(num_route = n()) %>%
  filter(num_route != 1) %>% nrow()
assert_that(many_to_one_shape_route == 0)

#check if  the edges in the transfer file are all within the same complex
#this is not the case <- we have to put the edges in to a graph and use cluster(g) to find actual station_groups
transfer %>% translate(stations, stop_id, complex_id, from_stop_id = from_complex_id, to_stop_id = to_complex_id) %>%
  filter(from_complex_id != to_complex_id) %>% translate(stops, stop_id, stop_name, from_stop_id,to_stop_id) %>% View

#check if that all edges are present in the transfer file
#this is not the case due to terminal stations
test.transfer_from_eq_to <- transfer %>% filter(from_stop_id == to_stop_id) %>% distinct 

stops %>% filter(location_type == 1 ) %>% distinct %>% select(stop_id, stop_name) %>% 
  left_join(test.transfer_from_eq_to, by =c('stop_id'='from_stop_id')) %>% filter(is.na(to_stop_id)) %>% View

save.mult('taxi_cleaned',paths, routes, shape_edges, shapes, shapes_info, stations, stop_times, stops, transfer, trip_edges,trips)
