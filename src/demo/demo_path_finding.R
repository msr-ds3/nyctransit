library(igraph)
library(tidyverse)
source('../path_finding.R')
load('../../data/igraph_edges.rdata')
stops <- read_csv('../../data/google_transit_subway_static/stops.txt')
stations <- read_csv('http://web.mta.info/developers/data/nyct/subway/Stations.csv')

igraph_edges <- mutate(igraph_edges, 'weight'=as.numeric(`50%`))

graph <- graph.data.frame(igraph_edges)

from <- 'R28'
to <- '132'

# #use this when from, to does not have direction
# x <- get_itinerary(graph, '132', 'R28', 5)
# get_itinerary_directed(graph,'R16S','LGA',4)

# x %>% group_by(itinerary_id) %>% summarize(path = paste(station, collapse = '->'), route_id = paste(line, collapse = '->'),
                                           # sum(as.numeric(weight[1:(length(weight)-1)])))
# y %>% group_by(itinerary_id) %>% summarize(path = paste(station, collapse = '->'), route_id = paste(line, collapse = '->'), 
#                                            sum(as.numeric(weight[1:(length(weight)-1)]))) 
#use get_itinerary_complex when the from, to are complex ids
z <- get_itinerary_complex(graph, 617,628,20,stations, stops)

# 
# path_with_attributes <- add_path_attributes(graph, 1, path[[1]])
# 
# paths_tibble <- combine_paths_to_tibble(graph, path)
# 
# path_distance(graph, path[[1]])
# path_distance(graph, path[[2]])
# 
# path_sorted <- sort_path(graph, path)

# vertices <- V(graph)$name
# vertices <- vertices[vertices != from]
# for (i in vertices) 
#   get_itinerary(graph,stops, from, i, 3)
# #get_itinerary(graph, stops, 'A27','132',10)
# generated <- get_itinerary(graph, stops, '120', '131', 10)
# write.csv(generated, file = "itineraries-1-train.csv", row.names = F)
# generated <- get_itinerary(graph, stops, '227', '131', 10)
# write.csv(generated, file = "itineraries-Sid-train.csv", row.names = F)
# 
# generated <- get_itinerary(graph, stops, '712', 'D18', 10)
# write.csv(generated, file = "itineraries-Phoebe-train.csv", row.names = F)
# 
# generated <- get_itinerary(graph, stops, '210', '131', 10)
# write.csv(generated, file = "itineraries-Amanda-train.csv", row.names = F)
# 
# generated <- get_itinerary(graph, stops, '401', '132', 10)
# write.csv(generated, file = "itineraries-Akbar-train.csv", row.names = F)
# 
# generated <- get_itinerary(graph, stops, 'M14', 'D18', 10)
# write.csv(generated, file = "itineraries-M-train.csv", row.names = F)
# 
# generated <- get_itinerary(graph, stops, '120', '130', 10)
# write.csv(generated, file = "itineraries-Red-Line-96-23-train.csv", row.names = F)
# 
# generated <- get_itinerary(graph, stops, 'A19', 'A30', 10)
# write.csv(generated, file = "itineraries-Orange-Line-96-23-train.csv", row.names = F)
