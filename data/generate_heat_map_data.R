library(tidyverse)
library(httr)
library(rgdal)

load('../../data/igraph_edges.rdata')
stops <- read_csv('../../data/google_transit_subway_static/stops.txt')
stops <- stops %>% select(stop_id, stop_lat, stop_lon, stop_name)
names(igraph_edges) <- c("nxt_stop_id", "stop_id", "route_id", "mean", "sd", "10%", "median","90%")
igraph_edges <- igraph_edges %>% select("stop_id", "nxt_stop_id", median, `90%`, "route_id") %>% filter(stop_id != "LGA" & stop_id!= "JFK" & stop_id != "140" & nxt_stop_id != "JFK" & nxt_stop_id != "LGA" & nxt_stop_id != "140")
igraph_edges$median <- as.numeric(igraph_edges$median)
igraph_edges$`90%` <- as.numeric(igraph_edges$`90%`)

load('../../data/wait_times_rush_hour.rdata')
routes <- read_csv('../../data/google_transit_subway_static/routes.txt')
routes$route_color <- replace_na(routes$route_color, "000000") 
routes <- routes %>%
  mutate(route_color = sprintf("#%s", route_color)) %>% select(route_id, route_color)
map_data_lats <- seq(min(stops$stop_lat), max(stops$stop_lat), by=0.002)
map_data_lons <- seq(min(stops$stop_lon), max(stops$stop_lon), by=0.002)
grid <- expand.grid(map_data_lats, map_data_lons) %>%
  select(lat = Var1, lon = Var2)

r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

filtered_grid_spdf <- grid
coordinates(filtered_grid_spdf) <- ~lon + lat
proj4string(filtered_grid_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(filtered_grid_spdf, nyc_neighborhoods)
filtered_grid <- cbind(grid, matches)
grid <- filtered_grid %>%
  filter((!is.na(neighborhood)) & ! (borough == "Staten Island"))

save(grid, igraph_edges, stops, wait_times_filter, file = "heat_map_data.RData")