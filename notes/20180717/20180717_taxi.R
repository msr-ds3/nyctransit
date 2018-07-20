source('../../src/taxi/Util/index.r')
load.mult('../../data/taxi_cleaned/')

edges_path <- stop_times %>% group_by(trip_id) %>% mutate(nxt.stop_id = lead(stop_id), nxt.arrival_time = lead(arrival_time),
                                                          time_to_next = diff.time(arrival_time, nxt.arrival_time)) %>%
  filter(!is.na(nxt.stop_id)) %>% select(trip_id, stop_id, nxt.stop_id, time_to_next) %>% 
  translate(trips, trip_id, path_id,trip_id = path_id) %>% ungroup

edges_path.2 <- edges_path %>% select(-trip_id) %>%  group_by(stop_id, nxt.stop_id, path_id) %>%
  summarize(time_to_next = mean(time_to_next)) %>% ungroup()

edges_path.3 <- edges_path.2 %>% 
  mutate(stop_id = paste(stop_id, path_id,sep='_'), nxt.stop_id = paste(nxt.stop_id, path_id, sep='_')) %>%
  select(-path_id) 

stop_parents <- edges_path.2 %>% gather(key = 'x', value = 'stop_id', stop_id, nxt.stop_id) %>% 
  mutate(stop_sub = paste(stop_id, path_id, sep='_')) %>% select(stop_id, stop_sub) %>% distinct


library(igraph)

transfers <-read('../../data/google_transit_subway_static/transfers.txt')

transfer_edges <- transfers %>% 
  left_join(stop_parents,by = c('from_stop_id'='stop_id')) %>% 
  left_join(stop_parents, by =c('to_stop_id' = 'stop_id')) %>% 
  filter(stop_sub.x != stop_sub.y) %>%
  select(stop_id = stop_sub.x, stop_id.nxt = stop_sub.y, time_to_next = min_transfer_time) %>% mutate(type = 'transfer') 
  

transfer_edges %>% 
  extract(stop_id,'.*_(.*)',into = 'path_id', remove = F) %>% 
  extract(stop_id.nxt, '.*_(.*)', into = 'path_id.nxt', remove = F) %>%
  translate(paths, path_id, route_id, path_id, path_id.nxt) %>% View
  
transfer_edges %>%\
