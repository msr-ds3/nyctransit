library(igraph)
setwd('~/projects/ds3/nyctransit/src/demo')
source('../path_finding.R')
load('../../data/igraph_edges.rdata')
stops <- read_csv('../../data/taxi_cleaned/stops.csv')
graph <- graph.data.frame(igraph_edges)

from <- 'A27'
to <- 'F18'
to2 <- '123'

# path <- list()
# path[[1]] <- shortest_name_path(graph, from, to)
# path[[2]] <- shortest_name_path(graph, from, to2)
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
for (i in vertices) k_shortest_yen(graph, from, i, 3)

#get_itinerary(graph, stops, 'A27','132',10)
