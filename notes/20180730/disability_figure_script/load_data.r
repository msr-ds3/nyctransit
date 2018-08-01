library(tidyverse)
library(httr)
library(rgdal)
stops <- read_csv('../../../data/google_transit_subway_static/stops.txt') %>% 
  select(stop_id, stop_lat, stop_lon, stop_name)

routes <- read_csv('../../../data/google_transit_subway_static/routes.txt') %>% 
  mutate(route_color = replace_na(route_color, '000000'), route_color = sprintf("#%s", route_color))

load('../../../data/igraph_edges.rdata')

accessible_stations <- read_csv('../../../data/accessible_stations.csv') [[1]]

r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

stations <- read_csv('http://web.mta.info/developers/data/nyct/subway/Stations.csv')

save(file = 'data.rdata', stops, routes, igraph_edges, igraph_edges_directed, igraph_edges_exceptions, igraph_edges_map,
     accessible_stations, r, nyc_neighborhoods, stations)
