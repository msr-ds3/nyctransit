library(tidyverse)
library(igraph)

#returns the shortest path from to 'to'
#when there is no path returns null instead of warning message
#also instead of returning vertex id, it returns vertex name
shortestPath <- function(graph, from, to){
  path <- tryCatch(get.shortest.paths(graph,from, to, mode = 'out', output = 'vpath'), warning = function(x) NULL)
  names(unlist(path$vpath))
}

#calculates the distance/length/sum of weights for a given path
#the path should be a list of vertex names. e.g c('a','b','c') 
distance <- function(graph, path) sum(E(graph, path=path)$weight)

#sorts the path
sortPath <- function(graph,paths) paths[paths %>% sapply(distance, graph=graph) %>% order]

#yen doesnt work when from and to are the same node
#this is due to the way get.shortest.paths works(it doesnt respect self loop edges)
k_shortest.yen <- function(graph, from, to, k){
  if (from == to) stop('from and to can not be the same(currently)!!!')
  A <- list(shortestPath(graph,from,to))
  if (k == 1) return (A)
  B <- list()
  for (k_i in 2:k){
    for(i in 1:(length(A[[k_i-1]])-1)){
      spurNode <- A[[k_i-1]][i]
      rootPath <- A[[k_i-1]][1:i]
      edgesToDelete <- list()
      for (p in A){
        if (all(p[1:i] == rootPath)) {
          edge <- paste(p[i], ifelse(is.na(p[i+1]),p[i],p[i+1]), sep = '|')
          edgesToDelete <- append(edgesToDelete, edge)
        }
      }
      edgesToDelete <- unique(edgesToDelete)
      t_g <- graph
      for (edge in edgesToDelete){
        t_g <- delete.edges(t_g, edge)
      }
      spurPath <- shortestPath(t_g,spurNode,to)
      if (!is.null(spurPath)){
        total_path <- c(rootPath[-i], spurPath)
        total_path <- path_adjuster(graph, from, to, total_path)
        total_path <- list(total_path)
        if (!total_path %in% B && !total_path %in% A) B[length(B)+1] <- total_path
      }
    }
    if (length(B) == 0) break
    B <- sortPath(graph, B)
    A[k_i] <- B[1]
    B <- B[-1]
  }
  A
}

path_adjuster <- function(graph, from, to, path){
  edges <- E(graph, path = path)
  directions <- edges$direction_id
  for (i in 1:(length(directions)-1))
  {
    if (directions[i] == 'T' && directions[i+1] == 'T'){
      edge2remove <- paste(path[i], path[i+1], sep = '|')
      graph <- delete.edges(graph,edge2remove)
      path <- path_adjuster(graph, from,to,shortestPath(graph,from,to))
      break
    }
  }
  path
}

#extracts route_ids, and direction_id from path
extract_data <- function(graph,i,path){
  edges <- E(graph, path=path)
  
  direction <- edges$direction_id
  direction <- c(direction,direction[length(direction)])
  
  line <- c(edges$route_ids,'end')
  weight <- c(edges$weight, 'end')
  
  tibble(itinerary_id = i, station = path, line, direction, weight)
}

#combines paths into a tibble
paths_to_tibble <- function(graph, paths) {
  paths.asTibble <- 1:length(paths) %>%
  lapply(function(i) extract_data(graph,i, paths[[i]])) %>%
  reduce(rbind) 
}

#wrapper around yen, processes yen's result so the itinery functon can interact with it
k_shortest_path <- function(graph, from, to, k) k_shortest.yen(graph, from, to, k) %>% paths_to_tibble(graph=graph)
  

#for demo go to the demo folder 


#===============================================================================================================

#### Load data

stops <- read_csv('../data/google_transit_subway_static/stops.txt')
route <- read_csv('../data/google_transit_subway_static/routes.txt')

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