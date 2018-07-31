library(data.table)
wd <- getwd()
#setwd(here::here())
source('../src/time.R')
load('modeled_wait_times.RData')

compute_edge_popularity <- function(edges) {edges %>%
  group_by(route_id, stop_id, nxt_stop_id) %>% 
  mutate(c = n())%>% group_by(route_id) %>%
  mutate(average = mean(c), relative = c/average)
}

weight_summary <- function(duration, quantiles){
  info <- list()
  info['mean'] <- mean(duration)
  info['sd'] <- sd(duration)
  quantiles <- quantile(duration, quantiles)
  info[names(quantiles)] <- quantiles
  info
}

create_edges <- function (scheduled_edges, realtime_edges, transfer_edges, quantiles = c(.1,.5,.9),
             include_day_of_week = NULL, time_range = NULL, cutoff = NULL, relative_cutoff = NULL,
             exclude_routes = NULL, wait_time = NULL, dow = NULL, tod = NULL){
  scheduled_edges <- compute_edge_popularity(scheduled_edges)
  
  if (!is.null(relative_cutoff)) scheduled_edges <- filter(scheduled_edges, relative >=relative_cutoff)
  if (!is.null(cutoff)) scheduled_edges <- filter(scheduled_edges, c >= cutoff)
  if (!is.null(include_day_of_week)) {
    scheduled_edges <- filter(scheduled_edges, day_of_week %in% include_day_of_week)
    realtime_edges <- filter(realtime_edges, day_of_week %in% include_day_of_week)
  }
  if (!is.null(exclude_routes)) scheduled_edges <- filter(scheduled_edges, !route_id %in% exclude_routes)
  if (!is.null(time_range)) {
    time_start <- as.time24(time_range[1])
    time_end <- as.time24(time_range[2])
    scheduled_edges <- filter(scheduled_edges, within.timeRange(start_trip_time, time_start, time_end))
    realtime_edges <- filter(realtime_edges, within.timeRange(start_trip_time, time_start, time_end))
  }
  scheduled_edges <- scheduled_edges %>% select(route_id, stop_id, nxt_stop_id) %>% distinct
  
  filtered_realtime <- left_join(scheduled_edges, realtime_edges, by = c('route_id','stop_id', 'nxt_stop_id'))
  filtered_realtime <- filtered_realtime %>% filter(!is.na(duration))
  filtered_realtime <- filtered_realtime %>% group_by(stop_id, nxt_stop_id) %>% mutate(route_id = paste0(unique(route_id), collapse = '_'))

  edges <- as.data.table(filtered_realtime)[, weight_summary(duration, quantiles), by = c('stop_id', 'nxt_stop_id','route_id')] %>%
  as.data.frame()
  
  if(! is.null(wait_time)){
    wait_times_filter <- wait_times %>% filter(day %in% dow & time_of_day == tod) %>% 
      mutate(stop_mta_id = substr(stop_mta_id, 1, 3)) %>%
    group_by(stop_mta_id) %>%
      summarize(wait_time_median = mean(pred_median_wait), wait_time_90 = mean(pred_90th_wait))
    
    transfer_edges <- transfer_edges %>% left_join(wait_times_filter, by = c("nxt_stop_id" = "stop_mta_id")) %>% 
      mutate(duration = duration + wait_time_median) %>% 
      select(-wait_time_median, -wait_time_90)
  }
  
  c_names <- names(edges)[c(-(1:3),-5)]
  transfer_edges[,c_names] <- transfer_edges[,'duration']
  transfer_edges <- transfer_edges %>% mutate(sd =0)
  transfer_edges <- select(transfer_edges, -duration)
  bind_rows(edges, transfer_edges)
}

create_map <- function(edges_directed) {
  # edges_directed %>%
  # mutate(stop_id_u = substr(stop_id, 1, nchar(stop_id)-1), nxt_stop_id_u = substr(nxt_stop_id, 1, nchar(nxt_stop_id)-1))  %>% group_by(stop_id_u, nxt_stop_id_u) %>% 
  # mutate(count = n()) %>% ungroup() %>% mutate( stop_id_u = ifelse(count == 1 | route_id == 'T', stop_id_u,stop_id),
  #                                               nxt_stop_id_u = ifelse(count == 1 | route_id == 'T', nxt_stop_id_u,nxt_stop_id)) 
  
  edges_directed %>% 
    mutate(stop_id_u = ifelse(route_id == 'T', stop_id, substr(stop_id, 1, nchar(stop_id)-1)), 
         nxt_stop_id_u = ifelse(route_id == 'T', nxt_stop_id, substr(nxt_stop_id, 1, nchar(nxt_stop_id)-1))) %>% 
    group_by(stop_id_u, nxt_stop_id_u) %>% mutate(count = n()) %>% ungroup() %>% 
    mutate(stop_id_u = ifelse(count == 1, stop_id_u, stop_id),
           nxt_stop_id_u = ifelse(count == 1, nxt_stop_id_u, nxt_stop_id))
}

deduplicate <- function(edge_map){
  column_length <- ncol(edge_map)
  nxt_stop_id_u_index <- column_length-1
  stop_id_u_index <- nxt_stop_id_u_index-1
  indices <- c(stop_id_u_index, nxt_stop_id_u_index, 3:(stop_id_u_index-1))
  igraph_edges <- edge_map[indices] %>% rename(stop_id = stop_id_u, nxt_stop_id = nxt_stop_id_u) %>% distinct
}
#demo


save_edges <- function(edges, name)
{
  map <- create_map(edges)
  deduplicate <- deduplicate(map)
  exceptions <- map %>% filter(count != 1, route_id != 'T')
  map <- map %>% select(stop_id, stop_id_u, nxt_stop_id, nxt_stop_id_u)
    # group_by(stop_id_u, nxt_stop_id_u) %>% mutate(x = stop_id == last(stop_id) & nxt_stop_id == last(nxt_stop_id)) %>%
    # filter(x == T) 
  
  file_name <- paste0(name, '.rdata')
  map_name <- paste0(name, '_map')
  exceptions_name <- paste0(name, '_exceptions')
  directed_name <- paste0(name, '_directed')
  
  assign(map_name,map)
  assign(exceptions_name, exceptions)
  assign(name, deduplicate)
  assign(directed_name, edges)
  
  
  save(file = file_name, list = c(map_name, exceptions_name, directed_name, name))
}


#make sure to run create_edge_data.r before running this
load('edges.rdata')
# realtime_edges <- realtime_edges %>% mutate(start_trip_time = as.numeric(start_trip_time) %% secondsIn24Hours)
#create edges
edges <- create_edges(scheduled_edges, realtime_edges, transfer_edges, 
                             cutoff = 5, relative_cutoff = .1, time_range = c('7:00:00','9:00:00'), 
                             include_day_of_week = 'Weekday', wait_time = 7, tod = "Morning rush", dow = seq(2,6))

save_edges(edges, 'igraph_edges')

#add airtrains
airtrain_edges <- list(c("702N", "LGAN", "AT", 360, 0, 360, 360, 360),
                       c("H03N", "JFKN", "AT", 600, 60, 480, 600, 720),
                       c("G06N", "JFKN", "AT", 480, 72, 420, 480, 600),
                       
                       c("LGAS", "702S", "AT", 360, 0, 360, 360, 360),
                       c("JFKS", "H03S", "AT", 600, 60, 480, 600, 720),
                       c("JFKS", "G06S", "AT", 480, 72, 420, 480, 600))%>%
  reduce(rbind) %>% as.data.frame()
names(airtrain_edges) <- names(edges)

igraph_edges <- rbind(edges, airtrain_edges)

save_edges(igraph_edges, 'transfer_igraph_edges')
