library(tidyverse)
library(lubridate)
library(tidyr)
library(httr)
library(rgdal)
library(broom)

load('taxi_data_2015_01-06_lite.Rdata')
taxi <- taxi_data_lite %>%
  rename(pickup_datetime = tpep_pickup_datetime, dropoff_datetime = tpep_dropoff_datetime)
rm(taxi_data_lite)

# limit to range of dates we're interested in
range_begin <- as.Date("2015-01-01")
range_end <- as.Date("2015-06-30")
taxi <- taxi %>%
  mutate(ymd_pickup = as.Date(pickup_datetime) ) %>% 
  filter(ymd_pickup >= range_begin & ymd_pickup <= range_end)

# pull out hour, day of week info
taxi <- taxi %>%
     mutate(pickup_hour = hour(pickup_datetime), dropoff_hour = hour(dropoff_datetime), day_of_the_week = wday(ymd_pickup, label = TRUE)) 

# get neighborhood shapefiles
get_nyc_neighborhoods <- function(){
  r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
  return(readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F))
}

# get nyc boundaries
nyc_neighborhoods <- get_nyc_neighborhoods()

# convert spatial dataframe to tidy dataframe
nyc_df <- tidy(nyc_neighborhoods)
min_lat <- min(nyc_df$lat)
max_lat <- max(nyc_df$lat)
min_lng <- min(nyc_df$long)
max_lng <- max(nyc_df$long)

# filter out pickups and dropoffs outside of nyc boundaries
taxi_clean <- filter(taxi, pickup_longitude >= min_lng & pickup_longitude <= max_lat & pickup_latitude >= min_lat & pickup_latitude <= max_lat &
                       dropoff_longitude >= min_lng & dropoff_longitude <= max_lat & dropoff_latitude >= min_lat & dropoff_latitude <= max_lat)

# join the neighborhood info for pickup
spdf <- as.data.frame(taxi_clean)
coordinates(spdf) <- ~pickup_longitude +pickup_latitude
proj4string(spdf) <- proj4string(nyc_neighborhoods)
matches <- over(spdf, nyc_neighborhoods)
taxi_clean <- cbind(taxi_clean, matches)


# rename fields created for pickup neighborhoods
taxi_clean <- rename(taxi_clean, pickup_neighborhood = neighborhood, pickup_borough=borough, pickup_boroughCode = boroughCode, pickup_X.id = X.id)

#  join the neighborhood info for dropoff
spdf <- as.data.frame(taxi_clean)
coordinates(spdf) <- ~dropoff_longitude +dropoff_latitude
proj4string(spdf) <- proj4string(nyc_neighborhoods)
matches <- over(spdf, nyc_neighborhoods)
taxi_clean <- cbind(taxi_clean, matches)

# rename dropoff neighborhood field
taxi_clean <- rename(taxi_clean, dropoff_neighborhood = neighborhood, dropoff_borough=borough, dropoff_boroughCode = boroughCode, dropoff_X.id = X.id)

rm(spdf, nyc_df, min_lng, min_lat, max_lng, max_lat)

# remove expensive columns we don't need
taxi_clean <- taxi_clean %>% select(-pickup_X.id, -dropoff_X.id)

taxi_clean <- taxi_clean %>%
  mutate(trip_time_in_sec = as.numeric(difftime(dropoff_datetime, pickup_datetime, units = "sec"))) %>%
  select(pickup_datetime, day_of_the_week, pickup_hour, trip_time_in_sec, trip_distance, matches('(longitude|latitude|neighborhood)'))

save(taxi_clean, file = "taxi_data_2015_01-06-lite_clean.Rdata")
