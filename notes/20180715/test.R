source('./src/taxi/Util/index.r')

load.mult('data/taxi_cleaned/')

trips %>% select(path_id, day_type, direction, first_arrival_time, period) %>%
  spread_key_mult('period', 'day_type', 'direction') %>% group_by(path_id) %>% mutate_all(.funs = function(x) ifelse(is.na(x),0,x)) %>%
  summarize_at(.vars = vars(-first_arrival_time, -path_id), .funs = mean) %>% 
  mutate_at(.vars = vars(-path_id), function(x) round(x*100,2)) %>% View
  

trips %>% select(path_id, day_type, direction, first_arrival_time, period) %>% distinct %>% View

#determine when the type of station for a trian
#e.g wether route_1 stops at station 1 at all time or partially and etc
path_timePeriod_percentage <-trips %>% select(path_id, day_type, direction, first_arrival_time, period) %>%
  mutate(time_period =paste(day_type,period,sep='_'))  %>% spread_key_mult('period', 'day_type','time_period') %>%
  mutate_all(.funs = function(x) ifelse(is.na(x),0,x)) %>% group_by(path_id) %>% summarize_at(.vars = vars(-first_arrival_time, -path_id), .funs = mean) %>%
  mutate_at(.vars = vars(-path_id), function(x) round(x*100,2))

weekdayPaths <- path_timePeriod_percentage %>% filter(Weekday == 100) %>% select(path_id, starts_with('Weekday_')) %>% select(path_id)
rush_hourPaths <- weekdayPaths %>% filter(rush_hour)

  summarize_at(.vars = vars(-first_arrival_time, -path_id), .funs = mean) %>% 
  mutate_at(.vars = vars(-path_id), function(x) round(x*100,2)) %>% View
spread_key('time_period') 
spread_key('time_period') %>%
  mutate_all(.funs = function(x) ifelse(is.na(x),0,x)) %>% View
  summarize_at(.vars = vars(-first_arrival_time, -path_id), .funs = mean) %>% 
  mutate_at(.vars = vars(-path_id), function(x) round(x*100,2)) %>% View
