library(dplyr)
library(readr)

########################################
# load todd's subway realtime data
########################################

# Sometimes gives a 'domain' error that can be solved by starting a new session of R
realtime <- read_csv("todd_subway_realtime.csv", col_types = cols(route_mta_id = col_factor(levels = NULL)))

save(realtime, file='todd_subway_realtime.RData')