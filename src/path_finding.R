library(igraph)
library(tidyverse)

shortest_name_path <- function(graph, src, dest){
  path <- suppressWarnings(get.shortest.paths(graph, src, dest, mode = 'out', output = 'vpath'))
  path <- names(path$vpath[[1]])
  if (length(path)==1) return(NULL)
  path
}

add_path_attributes <- function(graph, id, path){
  edges <- E(graph, path = path)

  direction <- edges$direction_id
  direction <- c(direction, direction[length(direction)])

  line <- c(edges$route_ids,'end')
  weight <- c(edges$weight, 'end')

  tibble(itinerary_id = id, station = path, line, direction, weight)

}

combine_paths_to_tibble <- function(graph, paths){
  1:length(paths) %>%
    lapply(function(i) add_path_attributes(graph,i, paths[[i]])) %>%
    reduce(rbind)
}

path_distance <- function(graph, path) sum(E(graph, path=path)$weight)

sort_path <- function(graph,paths) paths[paths %>% sapply(path_distance, graph=graph) %>% order]

find_edges_to_delete <- function(A, C,i,rootPath){
  edgesToDelete <- list()
  for (p in append(A,C)){
    rootPath_p <- p[1:i]
    if (all(rootPath_p == rootPath)){
      edge <- paste(p[i], ifelse(is.na(p[i+1]),p[i],p[i+1]), sep = '|')
      edgesToDelete <- append(edgesToDelete, edge)
    }
  }
  unique(edgesToDelete)
}

check_for_double_transfer <- function(graph, path){
  edges <- E(graph, path = path)
  directions <- edges$direction_id
  for (i in 1:(length(directions)-1))
  {
    if (directions[i] == 'T' && directions[i+1] == 'T') return (T)
  }
  F
}


k_shortest_yen <- function(graph, src, dest, k){
  if (src == dest) stop('src and dest can not be the same (currently)')

  #accepted paths
  A <- list(shortest_name_path(graph, src, dest))
  if (k == 1) return (A)
  #potential paths
  B <- list()
  #accepted paths but invalid due to double transfer or any other reasons
  C <- list()

  for (k_i in 2:k){
    prev_path <- A[[k_i-1]]
    num_nodes_to_loop <- length(prev_path)-1
    for(i in 1:num_nodes_to_loop){
      spurNode <- prev_path[i]
      rootPath <- prev_path[1:i]

      edgesToDelete <- find_edges_to_delete(A,C, i,rootPath)
      t_g <- graph
      for (edge in edgesToDelete) t_g <- delete.edges(t_g, edge)

      spurPath <- shortest_name_path(t_g,spurNode, dest)

      if (!is.null(spurPath)){
        total_path <- list(c(rootPath[-i], spurPath))
        #check for double transfer
        if (check_for_double_transfer(graph, total_path[[1]])){
          C[length(C)+1] <- total_path
        }else{
          if (!total_path %in% B) B[length(B)+1] <- total_path
        }
        
      }
    }
    if (length(B) == 0) break
    B <- sort_path(graph, B)
    A[k_i] <- B[1]
    B <- B[-1]
  }
  A
}

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

get_itinerary_raw <- function(shortest_paths_df, stops) {
  
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

get_itinerary <- function(graph, stops, src,dest, k){
 k_shortest_yen(graph, src, dest, k) %>% 
    combine_paths_to_tibble(graph = graph) %>% 
    greedy(k) %>%
    get_itinerary_raw(stops)
}