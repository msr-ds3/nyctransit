library(tidyverse)
library(igraph)
library(lubridate)

################# LOAD FILES #################

stops <- read_csv('google_transit_subway_static/stops.txt')
routes <- read_csv('google_transit_subway_static/routes.txt')
trips <- read_csv('google_transit_subway_static/trips.txt')
stop_times <- read_csv('google_transit_subway_static/stop_times.txt')
transfers <- read_csv('google_transit_subway_static/transfers.txt')

load('todd_subway_realtime.RData')

################# FILTER TIME INTERVALS #################

# all service
# time_filter <- seq(0, 23)
# day_filter <- c("Weekday", "Saturday", "Sunday")

# ordinary (weekday+daytime) service
time_filter <- seq(6, 23)
day_filter <- c("Weekday")

# weekend (daytime) service
#time_filter <- seq(6, 23)
#day_filter <- c("Saturday", "Sunday")

# late night (weekday+weekend) service
# time_filter <- seq(0, 5)
# day_filter <- c("Weekday", "Saturday", "Sunday")

############### FORMATTING STATIC  #######################

trips$route_id <- as.character(trips$route_id)

# all scheduled trips
all_trips <- stop_times %>%
  filter(!is.na(arrival_time)) %>%
  left_join(stops) %>%
  extract(trip_id, c("route_id"), regex=".*_.*_([^.]*)\\.\\.?.*", remove=FALSE) %>%
  extract(trip_id, c("day_of_week"), regex=".*-.*-(.*)-.*", remove=FALSE) %>%
  extract(trip_id, c("time"), regex=".*-.*-.*-.*_(.*)_.*\\.\\.?.*", remove=FALSE) %>%
  mutate(direction = substr(stop_id, 4, 4),
         stop_id = substr(stop_id, 1, 3),
         prev_stop_id = ifelse(trip_id == lag(trip_id), lag(stop_id), NA),
         prev_stop_name = ifelse(trip_id == lag(trip_id), lag(stop_name), NA),
         trip_start_time = seconds_to_period(as.numeric(time)*.6),
         trip_start_time = as.POSIXct(sprintf("%s:%s:%s", 
                                              hour(trip_start_time), minute(trip_start_time), second(trip_start_time)),
                                      "%H:%M:%S", tz="America/New_York")) %>%
  left_join(trips) %>%
  select(route_id, trip_id, direction_id = direction, day_of_week, trip_start_time, arrival_time, departure_time, 
         stop_id, stop_name, prev_stop_id, prev_stop_name)

################### Transfer formatting  ##################

transfer_sequences <- transfers %>% left_join(stops, by = c("to_stop_id" = "stop_id")) %>% 
  left_join(stops, by = c("from_stop_id" = "stop_id")) %>%
  mutate(route_ids = "T", direction_id = "T", weight = min_transfer_time, sd = NA, lower_quartile = NA, mean = NA, upper_quartile = NA) %>%
  select(route_ids, direction_id, stop_id = to_stop_id, stop_name = stop_name.x, prev_stop_id = from_stop_id, 
         prev_stop_name = stop_name.y, weight, sd, lower_quartile, mean, upper_quartile)

#### Realtime Formatting

realtime <- realtime %>%
  mutate(day_of_week = weekdays(departure_time),
         day_of_week = ifelse(day_of_week != "Saturday" & day_of_week != "Sunday",
                              "Weekday", day_of_week),
         direction_id = ifelse(direction == 1, "N", "S"))

### RERUN FROM THIS POINT ON IF TIME/DAY FILTER CHANGES
#### FILTERING FOR RARE TRIPS #####################

# unique trip sequences by line
filtered_sequences <- all_trips %>%
  filter(hour(trip_start_time) %in% time_filter) %>%
  filter(day_of_week %in% day_filter) 

counts <- filtered_sequences %>%
  group_by(route_id) %>%
  summarize(total = n())

percentages <- filtered_sequences %>%
  select(route_id, direction_id, stop_id, stop_name, prev_stop_id, prev_stop_name) %>%
  group_by(route_id, direction_id, stop_id, stop_name, prev_stop_id, prev_stop_name) %>%
  summarize(count = n()) %>%
  left_join(counts) %>%
  mutate(relative_percentage = count/total)

quartiles <- percentages %>%
  group_by(route_id) %>%
  summarize(lower_10 = quantile(relative_percentage, 0.1), lower = quantile(relative_percentage, 0.25))

percentages <- percentages %>%
  left_join(quartiles)

unique_sequences <- percentages %>%
  filter(relative_percentage >= lower_10) %>%
  select(route_id, direction_id, stop_id, stop_name, prev_stop_id, prev_stop_name) 

########### GET STATION WEIGHTS ####################

station_weights <- realtime %>%
  mutate(stop_mta_id = substr(stop_mta_id, 1, 3)) %>%
  
  arrange(realtime_trip_id, departure_time) %>% 
  mutate(travel_time = ifelse(realtime_trip_id == lag(realtime_trip_id),
                              departure_time - lag(departure_time), NA)) %>%
  mutate(prev_stop_mta_id = ifelse(realtime_trip_id == lag(realtime_trip_id),
                                   lag(stop_mta_id), NA)) %>%
  filter(!is.na(travel_time), 
         hour(departure_time) %in% time_filter,
         day_of_week %in% day_filter) %>% 
  group_by(stop_mta_id, prev_stop_mta_id) %>% 
  summarize(weight = median(travel_time), sd = sd(travel_time, na.rm=TRUE),
            lower_quartile = quantile(travel_time, 0.25),
            mean = mean(travel_time), upper_quartile = quantile(travel_time, 0.75))

######### COMBINE STATION IDs ##############
station_route_ids <- unique_sequences %>% 
  select(route_id, stop_id, stop_name, prev_stop_id, prev_stop_name, direction_id) %>% distinct() %>% filter(! is.na(prev_stop_id)) %>%
  spread(key = route_id, value = 1) %>% 
  unite("route_ids", -stop_id, -prev_stop_id, -direction_id, -stop_name, -prev_stop_name) %>%
  mutate(route_ids = gsub("(_NA)|(NA_)","", route_ids))

#### cOMBINE REALTIME WITH STATIC #############

# Static with realtime weights and route_ids
full_sequences <- station_route_ids %>%
  left_join(station_weights, by = c("stop_id" = "stop_mta_id", "prev_stop_id" =
                                      "prev_stop_mta_id"))%>%
  select(route_ids, direction_id, stop_id, stop_name, prev_stop_id, prev_stop_name, 
         weight, sd, lower_quartile, mean, upper_quartile)

# Transfers already has weights from the min_transfer_time field (now renamed weight) so we rbind the two sequence objects
full_sequences <- bind_rows(full_sequences, transfer_sequences)

################# MAKE iGRAPH ##############

# Input a dataframe of the edges that exist between stops, with weight and direction as attributes
igraph_edges <- full_sequences %>%
  filter(!is.na(prev_stop_id), !is.na(weight)) %>%
  select(prev_stop_id, stop_id, weight, route_ids, direction_id, stop_name)

mta_igraph <- graph.data.frame(igraph_edges, directed=TRUE)

################# SAVE IGRAPH #################
save(mta_igraph, file='mta_igraph.RData')
