library(tidyverse)
library(lubridate)

#removes empty columns
removeEmptyColumns <- function(table){
  logical_column_indices <- !sapply(table, function(x)all(is.na(x)))
  table[,logical_column_indices]
}

#
read <- function(name, asString=F)
{
  if (asString) table <- read_csv(name, col_types = cols(.default = col_character()))
  else table <- read_csv(name)
  names(table) <- gsub(' ', '_', names(table))
  table %>% removeEmptyColumns
}
convertTimeTo24 <- function(times)
{
  times %>% extract_numeric('(.*)(:.*:.*)', into = c('hour','remain')) %>% mutate()
}


stop_times <- read('google_transit_subway_static/stop_times.txt',T) %>% 
  mutate(stop_id.directed = stop_id, stop_id = substr(stop_id,1,nchar(stop_id)-1))

stop_times$stop_id <- as.factor(stop_times$stop_id)
stop_times$arrival_time

stop_times%>% extract(arrival_time,regex = '(.*)(:.*:.*)', into = c('hour','remain'),convert = T) %>%
  mutate(hour=ifelse(hour > 24, hour-24,hour), hour=str_pad(string=hour,width=2,pad='0'),arrival_time = paste(hour,remain,sep='')) %>% View
  

#(2[567])(?=:[0-9]{2}:[0-9]{2})
#(2[567])
