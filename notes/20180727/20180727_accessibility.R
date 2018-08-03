library(tidyverse)
library(igraph)
library(leaflet)

setwd(here::here())
accessibility <- read_csv('./data/accessible_station_ids.csv',col_types = cols('Complex ID' = col_integer()))
stations <- read_csv('http://web.mta.info/developers/data/nyct/subway/Stations.csv')

stations <-left_join(accessibility, stations) %>% select(stop_id =`GTFS Stop ID`) 
write_csv(stations, path = 'data/accessible_stations.csv')


