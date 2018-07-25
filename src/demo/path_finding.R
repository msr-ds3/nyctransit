library(igraph)
# setwd('~/projects/ds3/nyctransit/src/demo')
source('../path_finding.R')
load('../../data/igraph_edges.rdata')
stops <- read_csv('../../data/google_transit_subway_static/stops.txt')
graph <- graph.data.frame(igraph_edges)

from <- '128'
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

vertices <- V(graph)$name
vertices <- vertices[vertices != from]
for (i in vertices) 
  get_itinerary(graph,stops, from, i, 3)
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

generated <- get_itinerary(graph, stops, '120', '130', 10)
write.csv(generated, file = "itineraries-Red-Line-96-23-train.csv", row.names = F)

generated <- get_itinerary(graph, stops, 'A19', 'D19', 10)
write.csv(generated, file = "itineraries-Orange-Line-96-23-train.csv", row.names = F)

# Shortest Paths from Midtown to Jamaica (i.e. 5 Av/53 St (F12) => JFK Airport (G06))
generated <- get_itinerary(graph, stops, 'F12', 'G06', 10)
write.csv(generated, file = "itineraries-Midtown-Jamaica.csv", row.names = F)
