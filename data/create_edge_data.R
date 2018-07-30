library(tidyverse)
library(igraph)
library(lubridate)
source('../src/read.R')
source('../src/time.R')
source('../src/translate.R')

#---------------------raw data---------------------
trips <- read('google_transit_subway_static/trips.txt', c = 'route_id') %>%
  extract(col=trip_id,regex = '.*-.*-(.*)-.._(.*)_.*', into = c('day_of_week','origin_time'), remove = F)

stops <- read('google_transit_subway_static/stops.txt')

routes <- read('google_transit_subway_static/routes.txt')

stop_times <- read('google_transit_subway_static/stop_times.txt',c = c('arrival_time','departure_time')) %>%
  mutate(arrival_time_sec = as.time24(arrival_time), departure_time_sec = as.time24(departure_time)) %>%
  mutate(arrival_time = as.timeStr(arrival_time_sec), departure_time = as.timeStr(departure_time_sec))

transfers <- read('google_transit_subway_static/transfers.txt')

load('todd_subway_realtime.RData')


#---------------scheduled edges--------------------
#route_id, trip_id, direction_id, day_of_week
#start_trip_time, end_trip_time
#stop_id, nxt_stop_id, departure_time, nxt_departure_time, duration

#add nxt_stop_id, nxt_departure_time_sec
scheduled_edges <- stop_times %>%
  mutate(nxt_stop_id = lead(stop_id), 
         nxt_departure_time_sec = lead(departure_time_sec)) %>% 
  filter(!is.na(nxt_stop_id)) 

#add duration and start, end trip time
scheduled_edges <- scheduled_edges %>% 
  mutate(duration = diff.time(departure_time_sec, nxt_departure_time_sec)) %>%
  group_by(trip_id) %>% 
  mutate(end_trip_time =last(departure_time_sec), start_trip_time = first(departure_time_sec)) %>%
  ungroup()

#add day_of_week, route_id
scheduled_edges <- scheduled_edges %>% 
  translate(trips, trip_id, c(day_of_week, route_id),trip_id = c(day_of_week,route_id))

scheduled_edges <- select(scheduled_edges, 
                          route_id, trip_id, day_of_week,
                          start_trip_time, end_trip_time, 
                          stop_id, nxt_stop_id, 
                          departure_time = departure_time_sec, nxt_departure_time = nxt_departure_time_sec,
                          duration)

#---------------Transfer edges---------------------
#
transfer_edges <- transfers %>% 
  select(stop_id = from_stop_id, nxt_stop_id = to_stop_id, duration = min_transfer_time) %>%
  mutate(route_id = "T")
#add direction to stop_id
# transfer_edges_NS <- mutate(transfer_edges, stop_id = paste0(stop_id, 'N'), nxt_stop_id = paste0(nxt_stop_id,'S'))
# transfer_edges_SN <- mutate(transfer_edges, stop_id = paste0(stop_id, 'S'), nxt_stop_id = paste0(nxt_stop_id,'N'))
# transfer_edges_NN <- mutate(transfer_edges, stop_id = paste0(stop_id, 'N'), nxt_stop_id = paste0(nxt_stop_id,'N'))
# transfer_edges_SS <- mutate(transfer_edges, stop_id = paste0(stop_id, 'S'), nxt_stop_id = paste0(nxt_stop_id,'S'))
# transfer_edges <- rbind(transfer_edges_SN, transfer_edges_NS, transfer_edges_SS, transfer_edges_NN) %>%
#   mutate(route_id = 'T') %>% filter(stop_id != nxt_stop_id)
#---------------realtime edges---------------------
#reorder and rename stop_mta_id to stop_id
realtime_edges <- realtime %>% arrange(realtime_trip_id, departure_time) %>% rename(stop_id = stop_mta_id)


#add nxt_stop_id, nxt_departure_time, start_trip_time, end_trip_time and duration
realtime_edges <- realtime_edges %>% group_by(realtime_trip_id) %>% 
  mutate(nxt_stop_id = lead(stop_id), nxt_departure_time = lead(departure_time), start_trip_time = first(departure_time), end_trip_time = last(departure_time)) %>%
  filter(!is.na(nxt_stop_id)) %>% mutate(duration = nxt_departure_time - departure_time)
#add weekday
#note: I using start_trip_time instead of departure time because it might cause the same trip to end up with two day_of_week
realtime_edges <- realtime_edges %>% 
  mutate(day_of_week = weekdays(start_trip_time), day_of_week = ifelse(day_of_week %in% c('Saturday', 'Sunday'), day_of_week,'Weekday'))

realtime_edges <- realtime_edges %>% select(route_id = route_mta_id, trip_id = realtime_trip_id , day_of_week,
                                            start_trip_time, end_trip_time, 
                                            stop_id, nxt_stop_id,
                                            departure_time, nxt_departure_time,
                                            duration)

realtime_edges <- realtime_edges %>% mutate(start_trip_time = as.numeric(start_trip_time) %% secondsIn24Hours)

save(file = 'edges.rdata',scheduled_edges, transfer_edges, realtime_edges)

                               