source('../time.R')
source('../read.R')
stop_times <- read('../../../../data/google_transit_subway_static/stop_times.txt',asString = T)

library(tidyverse)

times <- stop_times %>% 
  gather(key='key', value='time', ends_with('time')) %>% 
  select(time) %>%
  distinct %>% 
  arrange(time) %>% 
  mutate(seconds= as.time24(time)) %>% #convert to seconds within 24 hours
  mutate(time.str = as.timeStr(time)) %>%
  mutate(period = categorize.timePeriod(seconds)) %>%
  mutate(period.fromStr = categorize.timePeriod(time.24))

times.filtered <- times %>% filter(within.timeRange(seconds, '10:30:00', '11:00:00'))

