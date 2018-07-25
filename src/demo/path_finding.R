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
generated <- get_itinerary(graph, stops, '120', '131', 10)
write.csv(generated, file = "itineraries-1-train.csv", row.names = F)
generated <- get_itinerary(graph, stops, '227', '131', 10)
write.csv(generated, file = "itineraries-Sid-train.csv", row.names = F)

generated <- get_itinerary(graph, stops, '712', 'D18', 10)
write.csv(generated, file = "itineraries-Phoebe-train.csv", row.names = F)

generated <- get_itinerary(graph, stops, '210', '131', 10)
write.csv(generated, file = "itineraries-Amanda-train.csv", row.names = F)

generated <- get_itinerary(graph, stops, '401', '132', 10)
write.csv(generated, file = "itineraries-Akbar-train.csv", row.names = F)

generated <- get_itinerary(graph, stops, 'M14', 'D18', 10)
write.csv(generated, file = "itineraries-M-train.csv", row.names = F)
