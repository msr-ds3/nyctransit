library(lubridate)
library(tidyverse)

#--------------------converting-----------------------------------------
secondsIn24Hours <- 24*60*60

#in: period object or string(character) in hms format.
#out: number of seconds from midnight(00:00:00)
as.time24 <- function(x)
{ 
  if (is.character(x)) x <- as.numeric(hms(x))
  else if (is.period(x)) x<- as.numeric(x)
  x %% secondsIn24Hours
}

#in: period object, number of seconds from midnight, string in hms format
#out:string hh:mm:ss in 24 hour format e.g 25:00:00 will be converted to 01:00:00
as.timeStr <- function(x){
  if(is.character(x)) x<-as.time24(x)
  if (!is.period(x)) x<-seconds_to_period(x)
  sprintf('%02d:%02d:%02d',x$hour,x$minute,x$.Data)
}

#-----------------------filter----------------------------------------

#in:
## x: the time column. doesnt work with date.time. can handle both seconds from midnight or hh:mm:ss string or period objects.
## from: begin of the time range
## to: end of the time range
#out: True if it is within , False if it is not
within.timeRange <- function(x, from, to)
{
  x <- as.time24(x)
  from <- as.time24(from)
  to <- as.time24(to)
  if (from <= to)
    return (from <= x & x <= to)
  return (from <= x | x <= to)
}
#--------------------------------categorize---------------------------
interval.timePeriod <-tibble(begin=c('6:30:00','9:30:00', '15:30:00','20:00:00','21:30:00','0:0:00','6:30:00')) %>%
  mutate(end = lead(begin)) %>% filter(!is.na(end))
interval.timePeriod$type <- c('rush.am', 'midday','rush.pm','evening.early','evening','latenight')

categorize.timePeriod <- function(x)
{
  categorize.timeInterval(x, interval.timePeriod)
}


categorize.timeInterval <- function(x, intervals)
{
  category <- c()
  for (i in 1:nrow(intervals))
  {
    interval <- intervals[i,]
    category[within.timeRange(x,interval$begin,interval$end)] <- interval$type
  }
  category
}

#--------------------------------timeDiff---------------------------
diff.time <- function(from,to){
  ifelse(from > to, secondsIn24Hours-from + to, to-from)
}
