source('../itinerary_evaluation.R')
load(file = "../../data/todd_subway_realtime.RData")

example_data <- read_csv('../../data/itineraries-1-train.csv') %>% 
  filter(event != "travel", itinerary_id < 4) %>%
  mutate(stop_station = paste(station, direction, sep = ''))

all_itin_df <- compute_all_itins(example_data, subway_data)
plot_densities(all_itin_df)
