library(data.table)
wd <- getwd()
setwd(here::here())
source('src/time.R')

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
             exclude_routes = NULL){
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
  
  c_names <- names(edges)[c(-(1:3),-5)]
  transfer_edges[,c_names] <- transfer_edges[,'duration']
  transfer_edges <- transfer_edges %>% mutate(sd =0)
  transfer_edges <- select(transfer_edges, -duration)
  bind_rows(edges, transfer_edges)
}

#demo
#make sure to run create_edge_data.r before running this
load('data/edges.rdata')
realtime_edges <- realtime_edges %>% mutate(start_trip_time = as.numeric(start_trip_time) %% secondsIn24Hours)

#filter edges
igraph_edges <- create_edges(scheduled_edges, realtime_edges, transfer_edges, 
                             cutoff = 5, relative_cutoff = .1, time_range = c('7:00:00','9:00:00'), 
                             include_day_of_week = 'Weekday') 
#save igraph edges
save(file = 'data/igraph_edges.rdata', igraph_edges)
setwd(wd)

#add airtrains
airtrain_edges <- list(c("702N", "LGAN", "AT", 6, 0, 6, 6, 6),
                       c("702S", "LGAN", "AT", 6, 0, 6, 6, 6),
                       c("H03N", "JFKN", "AT", 10, 1, 8, 10, 12),
                       c("H03S", "JFKN", "AT", 10, 1, 8, 10, 12),
                       c("G06N", "JFKN", "AT", 8, 1.2, 7, 8, 10),
                       c("G06S", "JFKN", "AT", 8, 1.2, 7, 8, 10),
                       c("LGAN", "702N", "AT", 6, 0, 6, 6, 6),
                       c("LGAN", "702S", "AT", 6, 0, 6, 6, 6),
                       c("JFKN", "H03N", "AT", 10, 1, 8, 10, 12),
                       c("JFKN", "H03S", "AT", 10, 1, 8, 10, 12),
                       c("JFKN", "G06N", "AT", 8, 1.2, 7, 8, 10),
                       c("JFKN", "G06S", "AT", 8, 1.2, 7, 8, 10),
                       c("702N", "LGAS", "AT", 6, 0, 6, 6, 6),
                       c("702S", "LGAS", "AT", 6, 0, 6, 6, 6),
                       c("H03N", "JFKS", "AT", 10, 1, 8, 10, 12),
                       c("H03S", "JFKS", "AT", 10, 1, 8, 10, 12),
                       c("G06N", "JFKS", "AT", 8, 1.2, 7, 8, 10),
                       c("G06S", "JFKS", "AT", 8, 1.2, 7, 8, 10),
                       c("LGAS", "702N", "AT", 6, 0, 6, 6, 6),
                       c("LGAS", "702S", "AT", 6, 0, 6, 6, 6),
                       c("JFKS", "H03N", "AT", 10, 1, 8, 10, 12),
                       c("JFKS", "H03S", "AT", 10, 1, 8, 10, 12),
                       c("JFKS", "G06N", "AT", 8, 1.2, 7, 8, 10),
                       c("JFKS", "G06S", "AT", 8, 1.2, 7, 8, 10))%>%
  reduce(rbind) %>% as.data.frame()
names(airtrain_edges) <- names(igraph_edges)

igraph_edges <- rbind(igraph_edges, airtrain_edges)
#save igraph edges
save(file = 'igraph_edges.rdata', igraph_edges)