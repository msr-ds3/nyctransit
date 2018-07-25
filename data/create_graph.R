source('../src/time.R')

#make sure to run create_edge_data.r before running this
#load('./edges.rdata')

compute_edge_popularity <- function(edges) {edges %>%
  group_by(route_id, stop_id, nxt_stop_id) %>% 
  mutate(c = n())%>% group_by(route_id) %>%
  mutate(average = mean(c), relative = c/average)
}

filter_edges <- function(scheduled_edges, include_day_of_week = NULL, time_range= NULL, cutoff = NULL, relative_cutoff = NULL, exclude_routes = NULL){
  scheduled_edges <- compute_edge_popularity(scheduled_edges)

   if (!is.null(relative_cutoff)) scheduled_edges <- filter(scheduled_edges, relative >=relative_cutoff)
   if (!is.null(cutoff)) scheduled_edges <- filter(scheduled_edges, c >= cutoff)
   if (!is.null(include_day_of_week)) scheduled_edges <- filter(scheduled_edges, day_of_week %in% include_day_of_week)
   if (!is.null(exclude_routes)) scheduled_edges <- filter(scheduled_edges, !route_id %in% exclude_routes)
   if (!is.null(time_range)) scheduled_edges <- filter(scheduled_edges, within.timeRange(start_trip_time, time_range[1], time_range[2]) )
   scheduled_edges %>% select(route_id, stop_id, nxt_stop_id) %>% distinct
}

weight_summary <- function(duration, quantiles){
  info <- list()
  info['mean'] <- mean(duration)
  info['sd'] <- sd(duration)
  quantiles <- quantile(duration, quantiles)
  info[names(quantiles)] <- quantiles
  info
}

add_weights <- function(edges, realtime_edges, quantiles) {
  filtered_realtime <- left_join(edges, realtime_edges, by = c('route_id','stop_id', 'nxt_stop_id'))
  #ignore na for now
  filtered_realtime <- filtered_realtime %>% filter(!is.na(duration))
  filtered_realtime <- filtered_realtime %>% group_by(stop_id, nxt_stop_id) %>% mutate(route_id = paste0(unique(route_id), collapse = '_'))
  as.data.table(filtered_realtime)[, weight_summary(duration, quantiles), by = c('stop_id', 'nxt_stop_id','route_id')] %>% as.data.frame()
}

add_transfer <- function(edges, transfer){
  c_names <- names(edges)[c(-(1:3),-5)]
  transfer[,c_names] <- transfer[,'duration']
  transfer <- transfer %>% mutate(sd =0)
  transfer <- select(transfer, -duration)
  bind_rows(edges, transfer)
}

#demo
#filter edges
igraph_edges <- scheduled_edges %>% filter_edges(cutoff = 5, relative_cutoff = .1) 
#add weights
igraph_edges <- igraph_edges %>% add_weights(realtime_edges, c(.1,.25,.5,.75,.9))
#add transfer edges 
igraph_edges <- igraph_edges %>% add_transfer(transfer_edges)
#save igraph edges
save(file = 'igraph_edges.rdata', igraph_edges)
