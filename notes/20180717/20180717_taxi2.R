stations <- read('http://web.mta.info/developers/data/nyct/subway/Stations.csv')
transfer <- read('../data/google_transit_subway_static/transfers.txt')

substops.raw <- stop_times %>% translate(trips, trip_id, c(route_id,path_id), trip_id = c(route_id, path_id)) %>% 
  select(stop_id, stop_id.u, route_id, path_id) %>% 
  mutate(stop_route_id = paste(stop_id, route_id, sep='_'), stopu_path_id = paste(stop_id.u, path_id, sep='_')) %>% 
  translate(stations, GTFS_Stop_ID, Complex_ID, stop_id.u = complex_id) %>% distinct

substops_route <- substops.raw %>% select(complex_id, stop_route_id)  %>% distinct
substops_path <- substops.raw %>% select(complex_id, stopu_path_id) %>% distinct
stop_complex <- substops.raw %>% select(complex_id, stop_id.u) %>% distinct

#create transfer edges weight
#reorganize clean_google_transit
