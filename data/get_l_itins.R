setwd(here::here())


#### Load libraries
library(igraph)
library(leaflet)
library(RColorBrewer)
library(geosphere)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(ggmap)
library(gstat)
library(raster)
library(mapview)
library(tidyverse)



#### Load data
stops <- read_csv('data/google_transit_subway_static/stops.txt')
stops <- stops %>% select(stop_id, stop_lat, stop_lon, stop_name)

routes <- read_csv('data/google_transit_subway_static/routes.txt')
routes$route_color <- replace_na(routes$route_color, "6D6E71") 
routes <- routes %>%
  mutate(route_color = sprintf("#%s", route_color))

stops_names <- stops %>%
  mutate(stop_id = substr(stop_id, 1, 3),
         stop_name = paste(stop_name, stop_id)) %>%
  select(stop_id, stop_name) %>%
  distinct()


source('src/path_finding.R')
load('data/igraph_edges.rdata')

r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)


### GET GRAPHS
#### with L
igraph_edges <- mutate(igraph_edges, 'weight'=`90%`)
graph <- graph.data.frame(igraph_edges)


#### without L
no_l_edges <- igraph_edges %>%
  filter(route_id != "L")
l_graph <-graph.data.frame(no_l_edges)


#### settings
# final destination station
dest <- "635" # 14th St ******
# # dest <- "103N"
# 
# # 5 mins walking from 96 St
# home_lat <- 40.79613
# home_lon <- -73.97688
# 
# # near L/canarsie
# home_lat <- 40.651501
# home_lon <- -73.895612
# 
# # metropolitan ave
# home_lat <- 40.712037 
# home_lon <- -73.894585 
# 
home_lat <- 40.714469
home_lon <- -73.957626

# starting location (random coordinate)
home <- data.frame(lat = c(home_lat), lon = c(home_lon))

# number of paths
k <- 1






#### distance function
distance <- function(graph, path) sum(E(graph, path=path)$weight)

#### geodistance (in km)
Distance <- function(LatA, LonA, LatB, LonB){
  scaling_factor <- 110.25
  x <- LatA - LatB
  y <- (LonA - LonB) * cos(40.75 * (pi)/180)
  
  return (scaling_factor*sqrt(x * x + y * y))
}




#### Get map coords (once we have the itinerary)
# retrieve coordinates and stop names for itineraries
get_map_data <- function(itinerary) {
  
  map_data <- itinerary %>%
    left_join(stops, by = c("station" = "stop_id", "stop_name" = "stop_name"))  %>%
    group_by(itinerary_id) %>%
    mutate(prev_station = lead(station), prev_stop_name = lead(stop_name),
           prev_stop_lat = lead(stop_lat), prev_stop_lon = lead(stop_lon)) %>%
    extract(line, c("route_id"), regex="([^_][^_]?)_?.*", remove=FALSE) %>%
    ungroup() %>%
    left_join(routes) %>%
    select(itinerary_id, station, line, weight, leg, event, event_id,
           stop_name, stop_lat, stop_lon, prev_station, prev_stop_name, prev_stop_lat,
           prev_stop_lon, route_color)
  
  return(map_data)
}



#### layered map function
map_an_itinerary <- function(home, start, itinerary) {
  
  black <- "#000000"
  # start/stop marker colors
  green <- "#468504"
  red <- "#cf3400"
  stop_rad <- 5
  line_weight <- 3
  
  map_df <- get_map_data(itinerary) %>%
    mutate(line = str_replace_all(line, "_", " "),
           mins = round(as.integer(weight)/60, 1))
  
  # map_df %>% View
  
  num_layers <- max(map_df$itinerary_id)
  home <- home[rep(seq_len(nrow(home)), each=3), ]
  
  map <- leaflet() %>%
    addTiles() %>%
    setView(median(map_df$stop_lon), median(map_df$stop_lat), zoom = 12) %>%
    addProviderTiles("CartoDB.Positron")
  
  for (i in 1:num_layers) {
    df <- map_df %>% filter(itinerary_id==i)
    
    # df %>% View
    
    map <- map %>%
      # add station markers
      addCircleMarkers(df$stop_lon, df$stop_lat, color = df$route_color,
                       popup = paste("<b>", df$stop_name, "</b>", "<br/>", df$line),
                       radius = stop_rad, stroke = FALSE, fillOpacity = 0.7,
                       group = as.character(i)) %>%
      # add start marker
      addCircleMarkers(lat = home$lat[i], lng = home$lon[i], color = green, radius = 8, opacity = 0.9,
                       popup = paste("<b>", "start location", "</b>", "<br/>",
                                     home$lat[i], home$lon[i]),
                       group = as.character(i)) %>%
      # add walk to first station
      addPolylines(lat = c(home$lat[i], df$stop_lat[nrow(df)]),
                   lng = c(home$lon[i], df$stop_lon[nrow(df)]),
                   dashArray = c(8),
                   color = black, weight = line_weight,
                   popup = paste("<b>", "walk", "</b>", "<br/>", start$walking_mins[i], "mins"),
                   group = as.character(i)) %>%
      # add end marker
      addCircleMarkers(lat = df$stop_lat[1], lng = df$stop_lon[1], color = red, radius = 8, opacity = 0.9,
                       popup = paste("<b>", "end location", "</b>", "<br/>",
                                     df$stop_name[1], "<br/>",
                                     "<b>", "total commute:", "</b>", start$total_time_mins[i], "mins"),
                       group = as.character(i))
    
    for (j in 1:nrow(df)) {
      if (j == 1 & df$event[j] == "transfer") {
        map <- map %>% 
          addPolylines(lat = c(df$stop_lat[j], df$prev_stop_lat[j]),
                       lng = c(df$stop_lon[j], df$prev_stop_lon[j]),
                       dashArray = c(8), 
                       color = black, 
                       weight = line_weight,
                       popup = paste("<b>", "walk", "</b>", "<br/>", df$mins[j], "mins"),
                       group = as.character(i))
      } else {
        map <- map %>%
          addPolylines(lat = c(df$stop_lat[j], df$prev_stop_lat[j]),
                       lng = c(df$stop_lon[j], df$prev_stop_lon[j]),
                       color = ifelse(df$event[j] == "start_transfer", black, df$route_color[j]),
                       popup = ifelse(df$event[j] == "start_transfer",
                                      paste("<b>", "transfer", "</b>",
                                            df$line[j+1], "to", df$line[j], "<br/>",
                                            df$mins, "mins"),
                                      paste("<b>", df$line[j], "</b>", "<br/>", df$mins[j], "mins")),
                       dashArray = ifelse(df$event[j] == "start_transfer", c(8), c(0)),
                       weight = line_weight,
                       group = as.character(i))
        
      }
    }
    
  }
  
  map <- map %>%
    addLayersControl(overlayGroups = as.character(seq(1:num_layers)),
                     options = layersControlOptions(collapsed = FALSE))
  return(map)
}





#### put it all together
get_map_with_walking <- function(home, dest, igraph) {
  map_data <- get.all.shortest.paths(igraph, dest,  mode = "out")
  
  map_data <- map_data$res %>% 
    lapply(function(x)data.frame(stop_id = names(x)[length(x)],
                                 distance <- distance(igraph, x), 
                                 path = paste0(names(x), collapse = '>'))) %>%
    reduce(rbind) 
  
  
  map_data <- map_data %>% left_join(stops) 
  
  
  names(map_data) <- c("stop_id", "distance", "path", "stop_lat", "stop_lon", "stop_name")
  
  map_data <- map_data %>%
    mutate(subway_mins = round(distance/60, 2))
  
  
  result <- map_data %>% 
    mutate(walking_time = 720*Distance(home$lat, home$lon, stop_lat, stop_lon), 
           total_time = (walking_time + distance),
           walking_mins = round(walking_time/60, 2), 
           total_time_mins = round(total_time/60, 2)) 
  
  # consider n_stops stops near home that lead to the smallest total time
  n_stops <- 3
  start <- result %>% arrange(total_time_mins) %>% head(n_stops)
  # start %>% View
  
  itin <- data.frame()
  itin_id <- 1
  for (i in 1:n_stops) {
    itin_n <- get_itinerary(igraph, dest, start$stop_id[i], k, stops)
    itin_n$itinerary_id <- itin_id
    itin_id <- itin_id + 1
    # itin_n %>% View
    itin <- rbind(itin, itin_n)
  }
  
  # itin %>% View
  
  map <- map_an_itinerary(home, start, itin)
  
  return(map)
}


