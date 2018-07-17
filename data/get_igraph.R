
library(tidyverse)
library(igraph)
library(lubridate)



stops <- read_csv('google_transit_subway_static/stops.txt')
routes <- read_csv('google_transit_subway_static/routes.txt')
trips <- read_csv('google_transit_subway_static/trips.txt')
stop_times <- read_csv('google_transit_subway_static/stop_times.txt')
transfers <- read_csv('google_transit_subway_static/transfers.txt')

load('todd_subway_realtime.RData')

#Ordinary Service
time_filter <- seq(6, 23)
day_filter <- c("Weekday")

#Weekend Service
#time_filter <- seq(6, 23)
#day_filter <- c("Saturday", "Sunday")

#Late Night Service
#time_filter <- seq(0, 5)
#day_filter <- c("Weekday", "Saturday", "Sunday")

################# STATIC -- formatting #################

trips$route_id <- as.character(trips$route_id)


# all scheduled trips
all_trips <- stop_times %>%
  filter(!is.na(arrival_time)) %>%
  left_join(stops) %>%
  extract(trip_id, c("route_id"), regex=".*_.*_(.*)\\.\\..*", remove=FALSE) %>%
  extract(trip_id, c("day_of_week"), regex=".*-.*-(.*)-.*", remove=FALSE) %>%
  extract(trip_id, c("time"), regex=".*-.*-.*-.*_(.*)_.*\\.\\..*", remove=FALSE) %>%
  mutate(stop_id = substr(stop_id, 1, 3),
         prev_stop_id = ifelse(trip_id == lag(trip_id), lag(stop_id), NA),
         prev_stop_name = ifelse(trip_id == lag(trip_id), lag(stop_name), NA),
         trip_start_time = seconds_to_period(as.numeric(time)*.6),
         trip_start_time = as.POSIXct(sprintf("%s:%s:%s", 
                                              hour(trip_start_time), minute(trip_start_time), second(trip_start_time)),
                                      "%H:%M:%S", tz="America/New_York")) %>%
  left_join(trips) %>%
  select(route_id, trip_id, direction_id, day_of_week, trip_start_time, arrival_time, departure_time, 
         stop_id, stop_name, prev_stop_id, prev_stop_name)


# unique trip sequences by line
unique_sequences <- all_trips %>%
  filter(hour(trip_start_time) %in% time_filter) %>%
  filter(day_of_week %in% day_filter) %>%
  select(route_id, stop_id, stop_name, prev_stop_id, prev_stop_name) %>%
  distinct 


# prepping the transfer data
transfer_sequences <- transfers %>% left_join(stops, by = c("to_stop_id" = "stop_id")) %>% 
  left_join(stops, by = c("from_stop_id" = "stop_id")) %>%
  mutate(route_id = "T", weight = min_transfer_time, sd = NA, lower_quartile = NA, median = NA, upper_quartile = NA) %>%
  select(route_id, stop_id = to_stop_id, stop_name = stop_name.x, prev_stop_id = from_stop_id, 
         prev_stop_name = stop_name.y, weight, sd, lower_quartile, median, upper_quartile)




################# REALTIME -- formatting #################
realtime <- realtime %>%
  mutate(day_of_week = weekdays(departure_time),
         day_of_week = ifelse(day_of_week != "Saturday" & day_of_week != "Sunday", "Weekday", day_of_week))

# get weights for the connections between stations
station_weights <- realtime %>%
  mutate(stop_mta_id = substr(stop_mta_id, 1, 3)) %>%
  arrange(realtime_trip_id, departure_time) %>% 
  mutate(travel_time = ifelse(realtime_trip_id == lag(realtime_trip_id), departure_time - lag(departure_time), NA)) %>%
  mutate(prev_stop_mta_id = ifelse(realtime_trip_id == lag(realtime_trip_id), lag(stop_mta_id), NA)) %>%
  filter(!is.na(travel_time)) %>% 
  group_by(route_mta_id, stop_mta_id, prev_stop_mta_id) %>% 
  summarize(weight = mean(travel_time), sd = sd(travel_time, na.rm=TRUE), lower_quartile = quantile(travel_time, 0.25),
            median = median(travel_time), upper_quartile = quantile(travel_time, 0.75))




################# COMBINE REALTIME WEIGHTS WITH STATIC EDGES -- USE THIS FOR IGRAPH #################

## STATIC WITH REALTIME WEIGHTS
full_sequences <- unique_sequences %>% 
  left_join(station_weights, by = c("stop_id" = "stop_mta_id", "prev_stop_id" = "prev_stop_mta_id", "route_id" = "route_mta_id"))


## ALL SEQUENCES -- STATIC & REALTIME
#Transfers already has weights from the min_transfer_time field (now renamed weight) so we rbind the two sequence objects
full_sequences <- rbind(full_sequences, transfer_sequences)


################# GET IGRAPH #################


igraph_edges <- full_sequences %>%
  filter(!is.na(prev_stop_id), !is.na(weight)) %>%
  select(prev_stop_id, stop_id, weight)

mta_igraph <- graph.data.frame(igraph_edges, directed=TRUE)
plot(mta_igraph)
  


################# SAVE IGRAPH #################
save(mta_igraph, file='ordinary_igraph.RData')