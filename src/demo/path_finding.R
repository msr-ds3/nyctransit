library(igraph)
library(here)
library(tidyverse)

source(here::here("src", "path_finding.R"))
load(here::here("data", "igraph_edges.rdata"))
stations <- read_csv('http://web.mta.info/developers/data/nyct/subway/Stations.csv')
stops <- read_csv(here::here("data", "google_transit_subway_static", "stops.txt"))

igraph_edges <- igraph_edges %>% mutate(weight = as.numeric(mean))
graph <- graph.data.frame(igraph_edges)

from <- '128'
to <- 'F18'

x <- get_itinerary(graph, '128','F18',2, map = igraph_edges_map, attributeNames = '90%')
x <- get_itinerary_complex(graph,'613','617',3, map = igraph_edges_map, stations)
map <- igraph_edges_map[,c('stop_id','nxt_stop_id','nxt_stop_id_u', 'stop_id_u')]


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
generated <- get_itinerary(graph, '120', '131', 10, stops, igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-1-train.csv"), row.names = F)
generated <- get_itinerary(graph, '227', '131', 10, stops, igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-Sid-train.csv"), row.names = F)

generated <- get_itinerary(graph, '712', 'D18', 10, stops, igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-Phoebe-train.csv"), row.names = F)

generated <- get_itinerary(graph, '210', '131', 10, stops, igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-Amanda-train.csv"), row.names = F)

generated <- get_itinerary(graph, '401', '132', 10, stops, igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-Akbar-train.csv"), row.names = F)

generated <- get_itinerary(graph, 'M14', 'D18', 10, stops, igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-M-train.csv"), row.names = F)

generated <- get_itinerary(graph, '120', '130', 10, stops,igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-Red-Line-96-23-train.csv"), row.names = F)

generated <- get_itinerary(graph, 'A19', 'D18', 10, stops, igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-Orange-Line-96-23-train.csv"), row.names = F)

# Shortest Paths from Midtown to Jamaica (i.e. 5 Av/53 St (F12) => JFK Airport (G06))
generated <- get_itinerary(graph, 'F12', 'G06', 10, stops, igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-Midtown-Jamaica.csv"), row.names = F)

# Shortest Paths from 96th to Fulton - Red Line (i.e. 96 St (235) => Fulton St (229))
generated <- get_itinerary(graph, '120', '229', 10, stops,igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-96-Fulton-red.csv"), row.names = F)

# Shortest Paths from 96th to Fulton - Red Line (i.e. 96 St (235) => Fulton St (229))
generated <- get_itinerary(graph, '625', '418', 10, stops,igraph_edges_map)
write.csv(generated, file = here("data", "itineraries-96-Fulton-green.csv"), row.names = F)


