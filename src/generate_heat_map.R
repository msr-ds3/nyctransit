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
library(KernSmooth)
library(ggplot2)
library(gstat)
library(raster)
library(tidyverse)
library(pdist)
load('../data/heat_map_data.RData')
source('path_finding.R')
distance <- function(graph, path) sum(E(graph, path=path)$weight)
Distance <- function(LatA, LonA, LatB, LonB){
  scaling_factor <- 110.25
  x <- LatA - LatB
  y <- (LonA - LonB) * cos(40.75 * (pi)/180)
  
  return (scaling_factor*sqrt(x * x + y * y))
}
generate_heat_map <- function(to){

  igraph_edges <- mutate(igraph_edges, 'weight'= median)
  mta_igraph <- graph.data.frame(igraph_edges)
#### Find shortest paths #####################
map_data <- get.all.shortest.paths(mta_igraph, to, mode = "out")

map_data <- map_data$res %>% lapply(function(x)data.frame(stop_id = names(x)[length(x)], distance <- distance(mta_igraph, x), path = paste0(names(x), collapse = '>'))) %>%
  reduce(rbind) 
map_data <- map_data %>% left_join(stops)
names(map_data) <- c("stop_id", "distance", "path", "stop_lat", "stop_lon", "stop_name")
map_data <- map_data %>% left_join(wait_times_filter, by = c("stop_id" = "stop_mta_id")) %>% mutate(distance = distance + wait_time_90)
map_data <- map_data %>%
  mutate(mins = distance%/%60,
         log_mins = log(mins))


#### Get subway travel time, walking time, total time
##### Filter for points reachable within 20 mins of walking
total_time = vector()
walking = vector()
station = vector()
station_id = vector()
for(i in 1:nrow(grid)){
  result<- map_data %>% mutate(walking_time = 720*Distance(grid$lat[i], grid$lon[i], stop_lat, stop_lon), total_time = (walking_time + distance))
  
  idx <- which.min(result$total_time)
  
  total_time[i] <- result[idx, "total_time"]
  walking[i] <- result[idx, "walking_time"]
  station[i] <- result[idx, "stop_name"]
  station_id[i] <- result[idx, "stop_id"]
}
grid$time <- total_time
grid$walking <- walking
grid$nearest_stop <- station
grid$stop_id <- station_id
grid <- grid %>% mutate(total_time_mins = time%/%60, walking_mins = walking%/%60, subway_mins = total_time_mins - walking_mins)
grid$log_mins <- log(grid$total_time_mins)


### **Combining with neighborhood**
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

filtered_grid <- grid %>%
  filter(walking_mins <= 20)

filtered_grid_spdf <- filtered_grid
coordinates(filtered_grid_spdf) <- ~lon + lat

coords <- filtered_grid[c("lon", "lat")]

gridded(filtered_grid_spdf) <- TRUE

grid_pal <- colorNumeric(
  palette = c('#720000','#ff0000','#ffff00', '#008000'),
  domain = (filtered_grid$total_time_mins),
  reverse = TRUE,
  na.color = "transparent")


#### Heat Map
#Interpolate surface and fix the output
idw <- idw(formula=(total_time_mins) ~ 1, locations=filtered_grid_spdf, newdata = filtered_grid_spdf)  # apply idw model for the data

residual_grid <- raster(idw, "var1.pred")
contourr <- rasterToContour(residual_grid)


rr <- mask(residual_grid, nyc_neighborhoods)
crs(rr) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

clrs <- brewer.pal(length(contourr@lines), "RdYlGn")

source <- filter(stops, stop_id == to)

m <- leaflet(nyc_neighborhoods) %>% 
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-73.89, 40.73, zoom = 11.25, options = "zoomSnap:.25") %>%
  addCircleMarkers(data = map_data, lng = ~stop_lon, lat = ~stop_lat,
                   color = ~grid_pal((mins)),
                   popup = paste("<b>", map_data$stop_name, "</b>", "<br/>",
                                 "subway:", map_data$mins, "mins", "<br/>"),
                   radius = 4, stroke = FALSE, fillOpacity =0.9) %>%
  addLegend("bottomright", pal = grid_pal, values = (map_data$mins),
            title = "Commute time",
            labFormat = labelFormat(suffix = " mins", transform = function(x) x),
            opacity = 1) %>%
  addMarkers(data = source, lng = ~stop_lon, lat= ~stop_lat)



heat_map <- addRasterImage(m, rr, color=grid_pal, opacity = 0.4)
heat_map
}

to <- "635"

library(profvis)
profvis({
map <- generate_heat_map(to)
})
map
