create_grid <- function(lat, lon, neighborhood, borough_blacklist = c(),  neighborhood_blacklist = c(), by = 0.002){
  grid_lats <- seq(min(lat), max(lat), by)
  grid_lons <- seq(min(lon), max(lon), by)
  grid <- expand.grid(grid_lats, grid_lons) %>%
    select(lat = Var1, lon = Var2)
  
  grid_spdf <- grid
  coordinates(grid_spdf) <- ~lon + lat
  proj4string(grid_spdf) <- proj4string(neighborhood)
  matches <- over(grid_spdf, neighborhood)
  grid$neighborhood <- matches$neighborhood
  grid$borough <- matches$borough
  grid %>% filter(!is.na(neighborhood) & !borough %in% borough_blacklist & !neighborhood %in% neighborhood_blacklist)
}

distance <- function(graph, path) sum(E(graph, path=path)$weight)

Distance <- function(LatA, LonA, LatB, LonB){
  scaling_factor <- 110.25
  x <- LatA - LatB
  y <- (LonA - LonB) * cos(40.75 * (pi)/180)
  
  return (scaling_factor*sqrt(x * x + y * y))
}

compute_point2station <- function(point2station_matrix,speed, k = 10, stop_whitelist = NULL, wait_time = NULL){
  if(!is.null(stop_whitelist)){
    point2station_matrix <- point2station_matrix %>% filter(stop_id %in% stop_whitelist)
  }
  point2station_matrix %>% 
  group_by(lat, lon, neighborhood, borough) %>% mutate(order = 1:length(stop_id)) %>%
  filter(order <= k) %>%  left_join(wait_time, by = 'stop_id') %>%
  mutate(travel_time = speed * distance, 
         subway_time = distance_matrix[stop_id, dest]/60 + weight/60,
         total_time = travel_time + subway_time,
         travel_ratio = travel_time/total_time) %>%
  arrange(total_time) %>%
  mutate(walking_distance = first(distance), nearest_stop = first(stop_id)) %>%
  filter(nearest_stop == stop_id)
}

create_pal <- function(min, max){
  colorNumeric(
    palette = c('#000000','#ff0000','#ffff00', '#008000'),
    domain = c(min, max),
    reverse = T,
    na.color = "transparent"
  )
}

create_map <- function(point2station_data, pal, color_source = point2station_data, color_col = 'total_time', dest, stops, position = "bottomright", title = "Commute time to", bins = 7){
  
  stop <- stops[stops$stop_id == dest,]
  stop_lon <- stop[[3]]
  stop_lat <- stop[[2]]
  stop_name <- stop[[4]]
  pal_col <- pull(color_source, color_col)

  leaflet(nyc_neighborhoods) %>%
    addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(-73.89,  40.73, zoom = 11.25, options = c('zoomSnap:.25')) %>%
    addCircleMarkers(lng = point2station_data$lon, lat = point2station_data$lat, opacity = 1, fillOpacity = 1,
                     color =pal(pal_col)) %>% 
    addMarkers(lng =stop_lon, lat = stop_lat) %>%
    addLegend(position, pal = pal,
              title = paste(title, stop_name), value = pal_col,bins = bins,
              labFormat = labelFormat(suffix = " mins"),
              opacity = 1) %>%
    htmlwidgets::onRender(
      'function(el, x){
    overlay = el.querySelector(".leaflet-overlay-pane")
    overlay.style.opacity = ".6"}'
    )
}
