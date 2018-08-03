library(tidyverse)
load('wait_times.RData')
tod = "Morning rush"
dow = seq(2,6)
wait_times_filter <- wait_times %>% filter(day %in% dow & time_of_day == tod) %>% 
  mutate(stop_mta_id = substr(stop_mta_id, 1, 3)) %>%
  group_by(stop_mta_id) %>%
  summarize(wait_time_median = mean(pred_median_wait), wait_time_90 = mean(pred_90th_wait))

save(wait_times_filter, file = "wait_times_rush_hour.rdata")