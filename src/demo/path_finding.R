library(igraph)
library(here)
library(tidyverse)

source(here::here("src", "path_finding.R"))
load(here::here("data", "at_igraph_edges.rdata"))
stations <- read_csv('http://web.mta.info/developers/data/nyct/subway/Stations.csv')
stops <- read_csv(here::here("data", "google_transit_subway_static", "stops.txt"))

igraph_edges <- at_igraph_edges %>% mutate(weight = as.numeric(mean))
graph <- graph.data.frame(igraph_edges)

from <- '128'
to <- 'F18'

x <- get_itinerary(graph, '128','JFK',2, map = at_igraph_edges_map, attributeNames = '90%')
x <- get_itinerary_complex(graph,'617','613',3, map = igraph_edges_map, stations = stations)

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

# Shortest Paths from Bryant Park to Mets Willets Point (5 Ave(724) => Mets Willets (702))
generated <- get_itinerary(graph, '724', '702', 10, stops)
write.csv(generated, file = here("data", "itineraries-5ave-MetsWillets.csv"), row.names = F)

# Shortest Paths from Complex IDs
# FORMAT: get_itinerary_complex(graph, start_complex, end_complex, k, map = igraph_edges_map, stations = stations)
# ==============================================================================

# Shortest Path from Atlantic Av (617) => Fulton (628)
super_generated <- get_itinerary_complex(graph, 617, 628, 10, map = at_igraph_edges_map, stations = stations, stops = stops)
write.csv(super_generated, file = here("data", "super-itin-atlantic-fulton.csv"))
# Shortest Path from Atlantic Av (617) => Union Square (602)
super_generated <- get_itinerary_complex(graph, 617, 602, 10, map = at_igraph_edges_map, stations = stations, stops = stops)
write.csv(super_generated, file = here("data", "super-itin-atlantic-union.csv"))
# Shortest Path from Atlantic Av (617) => Bryant Park (609)
super_generated <- get_itinerary_complex(graph, 617, 609, 10, map = at_igraph_edges_map, stations = stations, stops = stops)
write.csv(super_generated, file = here("data", "super-itin-atlantic-bryant.csv"))
# Shortest Path from Atlantic Av (617) => Grand Central (610)
super_generated <- get_itinerary_complex(graph, 617, 610, 10, map = at_igraph_edges_map, stations = stations, stops = stops)
write.csv(super_generated, file = here("data", "super-itin-atlantic-grand-central.csv"))
# Shortest Path from Atlantic Av (617) => Times Sq (611)
super_generated <- get_itinerary_complex(graph, 617, 611, 10, map = at_igraph_edges_map, stations = stations, stops = stops)
write.csv(super_generated, file = here("data", "super-itin-atlantic-times-square.csv"))
