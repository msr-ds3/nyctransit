
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
time_filter <- seq(9, 20)
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

# choose what train to filter over
route_filter <- "5"

# create the edges for the graph based on the given train
igraph_edges <- unique_sequences %>%
  filter(!is.na(prev_stop_id), route_id == route_filter) %>%
  select(prev_stop_id, stop_id)

# create the graph and plot it
mta_igraph <- graph.data.frame(igraph_edges, directed=TRUE)
plot(mta_igraph)

# get the name one of the first stops of the given train
first_stop <- "247"
 
# create the ordered list of stops based on the given train line
data.frame(stop_id = names(unlist(dfs(mta_igraph, first_stop)$order))) %>% left_join(stops) %>% select(stop_id, stop_name) %>% View()

################# SAVE IGRAPH #################
#save(mta_igraph, file='.RData')