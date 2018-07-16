source('./src/taxi/util.r')

#====================================routes========================================
#ignores the route_text_color since it is 99% na and useless
routes <- read('google_transit_subway_static/routes.txt',c('agency_id', 'route_text_color','route_url'))

#====================================shapes========================================
shapes <- read('google_transit_subway_static/shapes.txt')
shape_route<- shapes %>% select(shape_id) %>% distinct() %>% extract(shape_id, '(.*?)\\..*',into='route_id', F)
shape_route %>% select(route_id) %>% distinct() %>% View

#====================================Trips========================================
trips <-read('google_transit_subway_static/trips.txt')

#fills/fixes the route_id, and shape_id (these two columns sometimes contain na values)
#extracts type: Sunday, Saturaday, Weekday
#extracts realtime_trip_id for scheduled trips
#extracts orgin_time: seconds after midnight
#removes service_id since it is not very useful
trips.info <- trips %>% 
  select(-route_id,-service_id,-shape_id) %>% 
  extract(trip_id, '-.*-(.*?)-[0-9]{2}_(.*)_(.*)', into=c('type','origin_time','shape_id'), F,T) %>%
  extract(shape_id, '(.*?)\\..*',into='route_id', F) %>%
  extract(trip_id, '.*?_(.*)$',into='realtime_trip_id', F) %>%
  mutate_at(.vars=c('type','route_id','direction_id'),.funs=factor) %>% 
  mutate(origin_time=origin_time*.6)

#====================================Stop_times========================================
stop_times <- read('google_transit_subway_static/stop_times.txt') %>% 
  mutate(stop_id.directed = stop_id, stop_id = substr(stop_id,1,nchar(stop_id)-1))

edges <- stop_times %>% 
  group_by(trip_id) %>% 
  mutate(nxt = lead(stop_id,n=1)) %>%
  filter(!is.na(nxt)) %>% select(trip_id, cur=stop_id,nxt) %>% ungroup

edges.only <- edges %>%select(-trip_id) %>% distinct

paths_trips <- stop_times %>% group_by(trip_id) %>%
  mutate(path=paste0(stop_id, collapse = '>'),
         stop_id.start=first(stop_id),stop_id.end=last(stop_id), 
         arrival_time.start = first(arrival_time), arrival_time.end = last(arrival_time),
         departure_time.start = first(departure_time), departure_time.end = last(departure_time)) %>% 
  select(trip_id, path, ends_with('start'), ends_with('end')) %>% ungroup %>% distinct()

paths <- paths_trips %>% select(path,stop_id.start, stop_id.end) %>% distinct() %>% mutate(path_id = rank(path))

paths.info <- paths_trips %>% translate(trips.info, trip_id,c(type, route_id,direction_id), trip_id=c(type,route_id, direction_id)) %>%
  translate(paths, path,path_id,path=path_id)

paths.route <- paths.info %>% select(path_id, path, route_id,direction_id) %>% distinct #check if there is one to one relationship

timePeriod <- paths.info %>% 
  select (arrival_time.start, arrival_time.end, departure_time.start, departure_time.end)%>%
  gather('key','time',everything()) %>%
  select(time)%>%
  distinct() %>% 
  rowwise %>% mutate(period = getTimePeriod(time))

timePeriod.spread <- timePeriod %>% mutate(dummy=T) %>% spread(period,dummy) %>% sapply(function(x)replace_na(x,F))

paths.info <- paths.info %>% translate(timePeriod, time, period, arrival_time.start='period') 

paths.info %>% group_by(route_id, direction_id, path, path_id,period, type, stop_id.start, stop_id.end ) %>%
  summarize(n = n()) %>% filter(route_id == '7',direction_id=='0') %>% 
  translate(stops.undirected, stop_id, stop_name, stop_id.start, stop_id.end) %>%
  View
#express only run during weekdays
paths.info  %>% group_by(path_id, type, route_id) %>% summarize(count = n()) %>% 
  group_by(path_id) %>% 
  mutate(weekday_only= all(type=='Weekday')) %>%View

#test
paths.info %>% 
  group_by(path_id, type, period) %>% 
  summarize(c=n()) %>% mutate(dummy.1=T,dummy.2=T) %>% spread(type,dummy.1) %>% spread(period, dummy.2) %>%
  group_by(path_id) %>% mutate_all(.funs = function(x){any(x)}) %>% distinct() %>% 
  translate(paths.route,path_id, c(route_id, direction_id),path_id) %>%
  translate(paths.info, path_id, c(path,stop_id.start,stop_id.end), path_id=c(path,stop_id.start,stop_id.end)) %>%
  translate(stops.undirected, stop_id, stop_name, stop_id.start, stop_id.end) %>%
  View
#====================================Transfer========================================
transfers <- read('google_transit_subway_static/transfers.txt')

#===================================Stops==========================================
#removed parent_station since it is not very useful
stops <- read('google_transit_subway_static/stops.txt')

#undirected only shows the "parent stations"
stops.undirected <- stops %>% filter(location_type==1) %>% select(-location_type)

#===================================Stops==========================================
getMap <- function(dayType, time)
{
  
}


#time interval
#adjusted period if more than 90% is in rush.am and some are in latenight and some are in midday we adjust it as rush.am
#if more than 90% is in rush.pm and some are in midday to eve then adjust it as rush.pm
#columns needed: number of stops per station. 
#parent path: if the path is a subset of a path

#a train
#euclid <- the idea of late night depends on what day it is.

#multiple interval
#categorization system
#A Rush 
#<Route> <Branch> <Time> <Day> <Direction> <Start> <End>
#<-Rush->
#

#repair data
#goal create an id for each path
#determines which stop is express or local
#graph between points distance/time
#auto detects
#