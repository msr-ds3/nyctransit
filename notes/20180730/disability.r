distance <- function(graph, path) sum(E(graph, path=path)$weight)

Distance <- function(LatA, LonA, LatB, LonB){
  scaling_factor <- 110.25
  x <- LatA - LatB
  y <- (LonA - LonB) * cos(40.75 * (pi)/180)
  
  return (scaling_factor*sqrt(x * x + y * y))
}

create_grid <- function(lat, lon, neighborhood, by = 0.005){
  grid_lats <- seq(min(lat), max(lat), by=0.005)
  grid_lons <- seq(min(lon), max(lon), by=0.005)
  grid <- expand.grid(grid_lats, grid_lons) %>%
    select(lat = Var1, lon = Var2)
  
  grid_spdf <- grid
  coordinates(grid_spdf) <- ~lon + lat
  proj4string(grid_spdf) <- proj4string(neighborhood)
  matches <- over(grid_spdf, neighborhood)
  grid$neighborhood <- matches$neighborhood
  grid$borough <- matches$borough
  grid %>% filter(!is.na(neighborhood))
}

distance_station2station <- function(igraph, stops, dest, src = V(igraph)) {
  map_data <- get.all.shortest.paths(igraph, dest,  to = src,mode = "out")$res %>%
    lapply(function(x)data.frame(stop_id = names(x)[length(x)],
                                 time_seconds <- distance(igraph, x), 
                                 path = paste0(names(x), collapse = '>'))) %>%
    reduce(rbind)
  
  map_data <- map_data %>% left_join(stops) 
  names(map_data) <- c("stop_id", "time_seconds", "path", "stop_lat", "stop_lon", "stop_name")
  
  
  map_data <- map_data %>%
    mutate(subway_mins = time_seconds%/%60)
  
  return(map_data)
}

# distance_station2station(graph_mean, stops, 'A31', src = accessible_stations) %>% View

# get time from each point on grid to "dest station" 
distance_point2station <- function(map_data, grid, speed = 12) {
  speed <- speed * 60
  merge(grid, map_data, by = NULL) %>%
    mutate(walking_time = speed * Distance(lat, lon, stop_lat, stop_lon),
           total_time = (walking_time + time_seconds)) %>% group_by(lat, lon) %>% arrange(total_time) %>% 
    summarize(time = first(total_time), walking = first(walking_time),nearest_stop = first(stop_name)) %>%
    mutate(total_time_mins = time%/%60, walking_mins = walking%/%60, subway_mins = total_time_mins - walking_mins)
}

create_palette <- function(paletteSource, min_range = min(paletteSource$total_time_mins), max_range = max(paletteSource$total_time_mins)){
  clrs <- brewer.pal(11, "RdYlGn")
  
  colorNumeric(
    palette = clrs,
    domain = sqrt(c(min_range, max_range)),
    reverse = TRUE,
    na.color = "transparent")
}

add_heatmap_legend <- function(map, dest, palette, paletteSource, position = "bottomright", title = 'Commute time to'){
  addLegend(map, position, pal = palette,
            title = paste(title, dest), value = sqrt(paletteSource$total_time_mins),
            labFormat = labelFormat(suffix = " mins", transform = function(x) x*x),
            opacity = 1)
}

add_heatmap_stop_marker <- function(map, stop_id, stops){
  stop <- stops[stops$stop_id == stop_id,]
  stop_lon <- stop[[3]]
  stop_lat <- stop[[2]]
  addMarkers(map, lng =stop_lon, lat = stop_lat)
}

alternative_map <- function(map_data, dest, stops, palette = NULL, paletteSource = map_data, title = 'Commute time to'){
  
  if(is.null(palette)){
    palette <- create_palette(paletteSource)
  }
  
  map_data <- arrange(map_data, desc(total_time_mins))
  
  leaflet(nyc_neighborhoods) %>%
    addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(-73.98, 40.75, zoom = 11) %>%
    add_heatmap_legend(dest, palette, paletteSource, title = title) %>%
    addCircleMarkers(lng = map_data$lon, lat = map_data$lat, opacity = 1, fillOpacity = 1,
                     color =palette(sqrt(map_data$total_time_mins))) %>% 
    add_heatmap_stop_marker(dest, stops) %>%
    htmlwidgets::onRender(
      'function(el, x){
      overlay = el.querySelector(".leaflet-overlay-pane")
      overlay.style.opacity = ".6"}'
    )
}

diff_map <- function(point2station_A, point2station_B, dest, stops, max = NULL){
  time_diff <- point2station_A$total_time_mins - point2station_B$total_time_mins
  point2station_diff <- select(point2station_A, lat, lon)
  point2station_diff$total_time_mins <- time_diff
  if (!is.null(max)) point2station_diff <- filter(point2station_diff, total_time_mins < max)
  alternative_map(point2station_diff,dest = dest, stops = stops, title = 'Difference in time to')
}
