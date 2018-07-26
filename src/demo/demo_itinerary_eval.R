library(here)



source(here("src", "itinerary_evaluation.R"))
load(file = here("data", "todd_subway_realtime.RData"))

example_data <- read_csv(here("data", "itineraries-Akbar-train.csv")) %>% 
  filter(event != "travel", itinerary_id < 4)

all_itin_df <- compute_all_itins(example_data, subway_data)
plot_densities(all_itin_df)
