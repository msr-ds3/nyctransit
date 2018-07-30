
library(readr)
library(tidyverse)
library(lubridate)
library(glmnet)
library(broom)

load("../../data/todd_subway_realtime.RData")
todd_subway_data <- realtime
rm(realtime)

# organize weather data and station level data 

# load station_data from mta
station_data <- read_csv("http://web.mta.info/developers/data/nyct/subway/Stations.csv")

# get stop_id and stop_name fields, and create a stop_id with N and S appended
station_data <- station_data %>% 
  rename(stop_id = `GTFS Stop ID`, stop_name = `Stop Name`, structure = `Structure`, 
         lat = `GTFS Latitude`, long = `GTFS Longitude`) %>%
  mutate(stop_id_N = sprintf('%sN', stop_id), stop_id_S = sprintf('%sS', stop_id)) %>%
  gather(`stop_id_N`, `stop_id_S`, key = "stop_ids", value = "stop_id") %>%
  select(stop_name, stop_id, structure, lat, long)

# https://www.ncdc.noaa.gov/cdo-web/datatools/lcd         hourly weather data
weather_2018 <- read_csv("../../data/weather_2018.csv") %>% 
  mutate(ymd = as.Date(DATE), hour = hour(DATE)) %>%
  select(ymd, DATE, hour, HOURLYPrecip)

# replace T's, remove "s" from the end of entries, and remove NA
weather_2018$HOURLYPrecip[weather_2018$HOURLYPrecip == "T"] <- "0.001" # T = trace amounts(drizzling)

weather_2018$HOURLYPrecip  <- gsub("[^0-9\\.]","",weather_2018$HOURLYPrecip) # s = suspected precip

weather_2018 <- weather_2018 %>% 
  group_by(ymd) %>% arrange(ymd, hour) %>% 
  mutate(HOURLYPrecip = na.approx(HOURLYPrecip, rule = 2)) %>%
  ungroup()

# exp wait time for each station by station type for each day and hour
todd_subway_data <- todd_subway_data %>%
  mutate(ymd = as.Date(departure_time), 
         hour = hour(departure_time), 
         time_of_day = cut(hour(departure_time), c(0, 5, 10, 15, 20, 24), include.lowest = T, 
                           labels = c('Early morning', 'Morning rush', 'Mid-day', 'Evening rush', 'Night time')), 
         day = wday(departure_time, label = TRUE))

exp_wait_time_by_station <- todd_subway_data %>% 
  group_by(ymd, day, time_of_day, route_mta_id, stop_mta_id) %>% 
  summarise(perc90 = quantile(seconds_until_next_departure, .9), 
            median = quantile(seconds_until_next_departure, .5)) %>%
  ungroup()

precip <- weather_2018 %>% 
  mutate(time_of_day = cut(hour, c(0, 5, 10, 15, 20, 24), 
                           include.lowest = T, 
                           labels = c('Early morning', 'Morning rush', 'Mid-day', 'Evening rush', 'Night time'))) %>% 
  group_by(time_of_day, ymd) %>%
  summarize(avg_precip = mean(HOURLYPrecip)) %>%
  ungroup()

exp_wait_time_and_weather <- 
  left_join(exp_wait_time_by_station, precip, by = c("ymd", "time_of_day"))

# 90th percentile wait times by interval times of day

x <- sparse.model.matrix(perc90 ~ as.factor(stop_mta_id) + 
                           as.factor(route_mta_id) + as.factor(day) + 
                           as.factor(time_of_day), data = exp_wait_time_and_weather)
y <- exp_wait_time_and_weather$perc90

model <- glmnet(x, y, alpha = 0 , lambda = 0) 

# median wait times by interval times of day

x2<- sparse.model.matrix(median ~ as.factor(stop_mta_id) + 
                            as.factor(route_mta_id) + as.factor(day) + 
                            as.factor(time_of_day), data = exp_wait_time_and_weather)
y2 <- exp_wait_time_and_weather$median


model2 <- glmnet(x2, y2, alpha = 0 , lambda = 0)

# wait times df
wait_times <- exp_wait_time_and_weather %>% left_join(station_data, by = c("stop_mta_id" = "stop_id")) %>%
  select(route_mta_id, stop_mta_id, stop_name, day, time_of_day, avg_precip, lat, long) %>%
  mutate(route_mta_id = as.factor(route_mta_id),
         stop_mta_id = as.factor(stop_mta_id),
         day = as.factor(day),
         hour = as.factor(time_of_day)) %>%
  filter(avg_precip == 0) %>%
  arrange(stop_mta_id) %>%
  distinct()

wait_times_matrix <- sparse.model.matrix(~ stop_mta_id +
                                           route_mta_id + day +
                                           time_of_day, data = wait_times)

wait_times$pred_median_wait <- as.vector(predict(model2, newx = wait_times_matrix, s = "lambda.min"))
wait_times$pred_90th_wait <- as.vector(predict(model, newx = wait_times_matrix, s = "lambda.min"))