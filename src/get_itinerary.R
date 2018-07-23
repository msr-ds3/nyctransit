library(tidyverse)
library(igraph)
library(lubridate)

#### Load data

stops <- read_csv('../../data/google_transit_subway_static/stops.txt')
route <- read_csv('../../data/google_transit_subway_static/routes.txt')
source('../../src/k_shortest.R')

# time/day filtering happens in '../../data/get_igraph.R'
# if necessary, change filters there and rerun script before running next line


########### GREEDY FUNCTION ###################
greedy <- function(shortest_paths_df, num_itineraries){
  all_lines <- vector(mode = "character")
  
  for(i in 1:max(shortest_paths_df$itinerary_id)){
    
    df <- shortest_paths_df %>% 
      filter(itinerary_id == i)
    
    lines <- vector(mode = "character")
    
    # keep track of how many prev_line_ids to append
    r = 0
    
    prev_line_ids <-  str_split(df$line[i], "_")[[1]]
    
    for (j in 1:nrow(df)) {
      
      cur_line_ids <- str_split(df$line[j], "_")[[1]]
      intersect_lines <- intersect(cur_line_ids, prev_line_ids)
      
      if (length(intersect_lines) == 0){
        
        if("T" %in% cur_line_ids){
          
          # df[j,]$direction <- df[j-1,]$direction
        }
        
        else{
          # otherwise set to current line_ids
          intersect_lines <- cur_line_ids
        }
        
        # fill in all of the common lines up until the transfer
        lines <- append(lines, rep(paste(prev_line_ids, collapse = "_"), r))
        r = 0
      }
      
      r = r+1
      prev_line_ids <- intersect_lines
      
    }
    
    all_lines <- append(all_lines, lines)
    all_lines <- append(all_lines, "END")
    
  } # endfor
  shortest_paths_df$line <- all_lines
  
  return(shortest_paths_df)
  # return(all_lines)
  
}


############ GET FORMATTED ITINERARIES #################

get_itinerary <- function(shortest_paths_df) {
  
  # new df for the formatted itineraries
  itinerary <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                        c("itinerary_id", "station", "line", "direction", "leg",
                          "event", "event_id", "weight"))
  
  # get correct lines with greedy function
  shortest_paths_df <- greedy(shortest_paths_df)
  
  # format each itinerary_id separately
  for (i in 1:max(shortest_paths_df$itinerary_id)) {
    df <- shortest_paths_df %>%
      filter(itinerary_id == i)
    
    first_row <- NULL
    if(df$direction[1] == 'T'){
      first_row <- df[1:1,]
      first_row$leg <- 0
      first_row$event <- "transfer"
      first_row$event_id = 0
      df<- df[2:nrow(df),]
    }
    
    # current index
    k = 1
    
    # keep track of leg
    l = 1
    
    df <- df %>% mutate(leg = l, event = "travel")
    df$event[1] <- "start_trip"
    
    # compare current and next rows for whole itinerary
    while (k < nrow(df)){
      # next index
      j = k+1
      
      if (df$direction[k] == "T" & k == 1) {
        df$direction[k] <- "T"
      }
      
      # identify transfers
      else if (df$station[k] != df$station[j] & df$line[k] != df$line[j]) {
        
        # identify an 'implicit transfer (e.g. transfer 120->120 from 1 to 2/3)
        if (df$line[j] != "") {
          df <- df %>% add_row(itinerary_id = df$itinerary_id[j], station = df$station[j],
                               line = df$line[k], direction = df$direction[k], leg = l, weight = 0, .after = k)
        }
        
        # identify an 'explicit' transfer (e.g. transfer R11->629 from N/R to 4)
        else {
          df$line[j] <- df$line[k] 
          df$leg[j] <- df$leg[k]
        }
        
        # make note of transfer events
        df$event[j] <- "start_transfer"
        df$event[j+1] <- "end_transfer"
        
        # start_transfer direction gets inherited from previous station
        df$direction[j] <- df$direction[k]
        
        # skip newly added row
        k = k+1
        
        # new leg starts with a transfer
        l = l+1
        
      }
      
      k = k+1
      df$leg[k] <- l
      
    }
    
    # very last row is redundant
    if (df$station[nrow(df)] == df$station[nrow(df)-1]) {
      df <- df[1:nrow(df)-1,]
    }
    else{
      df$line[nrow(df)] <- ""
    }
    df$event[nrow(df)] <- "end_trip" 
    df <- df %>% mutate(event_id = seq(1:nrow(df)))
    
    if(! is.null(first_row)){
      df <- rbind(first_row, df)
      first_row <- NULL
    }
    
    # put it all together
    itinerary <- rbind(itinerary, df)
    
  }
  itinerary <- itinerary %>%
    left_join((stops %>% select(stop_id, stop_name)), by=c("station" = "stop_id"))
  
  return(itinerary)
}
