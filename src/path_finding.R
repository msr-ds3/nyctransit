library(igraph)
library(tidyverse)

shortest_path <- function(graph, src, dest){
  path <- suppressWarnings(get.shortest.paths(graph, src, dest, mode = 'out', output = 'vpath'))
  path <- names(path$vpath[[1]])
  if (length(path)==1) NULL else path
}

combine_paths_to_tibble <- function(paths, graph, attributeNames){
  if (is.null(paths[[1]])) return (NULL)
  r <- NULL
  for(i in 1:length(paths)){
    path <- paths[[i]]
    edges <- E(graph, path = path)
    attrs <- edge.attributes(graph, edges)
    t <- tibble(itinerary_id = i, station = path)
    for (name in c('route_id','weight', attributeNames))
      t[,name] <- c(attrs[[name]], 'end')
    r <- rbind(r, t)
  }
  rename(r, line = route_id)
}

path_weight <- function(graph, path) sum(E(graph, path=path)$weight)

sort_paths <- function(graph,paths) paths[paths %>% sapply(path_weight, graph=graph) %>% order]

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

is_significant_deviation <- function(graph, deviationPath){
  edges <- E(graph, path = deviationPath)
  route_ids <- edges$route_id
  loop <- length(deviationPath) -2
  for (i in 1:loop){
    if (route_ids[i] == 'T' && are.connected(graph, deviationPath[i], deviationPath[i+2])){
      return(F)
    }
  }
  T
}

k_shortest_yen <- function(graph, src, dest, k){
  if (src == dest) stop('src and dest can not be the same (currently)')

  #accepted paths
  A <- list(shortest_path(graph, src, dest))
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

      spurPath <- shortest_path(t_g,spurNode, dest)
      if (!is.null(spurPath)){
        total_path <- list(c(rootPath[-i], spurPath))
        
        if (is_significant_deviation(graph, total_path[[1]])){
          if (!total_path %in% B) B[length(B)+1] <- total_path
        }else{
          C[length(C)+1] <- total_path
        }
        
      }
    }
    if (length(B) == 0) break
    B <- sort_paths(graph, B)
    A[k_i] <- B[1]
    B <- B[-1]
  }
  A
}

greedy <- function(shortest_paths_df){
  all_lines <- vector(mode = "character")
  
  for(i in 1:max(shortest_paths_df$itinerary_id)){
    
    df <- shortest_paths_df %>%
      filter(itinerary_id == i)
    
    lines <- vector(mode = "character")
    
    # keep track of how many prev_line_ids to append
    r = 0
    prev_line_ids <-  str_split(df$line[i], "_")[[1]]
    prev_line_ids <- ifelse(prev_line_ids == "T", "", prev_line_ids)
    
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

format_itinerary_raw <- function(shortest_paths_df) {
  
  # new df for the formatted itineraries
  itinerary <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                        c("itinerary_id", "station", "line", "leg",
                          "event", "event_id", "weight"))
  
  # get correct lines with greedy function
  shortest_paths_df <- greedy(shortest_paths_df)
  
  # format each itinerary_id separately
  for (i in 1:max(shortest_paths_df$itinerary_id)) {
    df <- shortest_paths_df %>%
      filter(itinerary_id == i)
    
    first_row <- NULL
    if(df$line[1] == ""){
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
      
      #if (df$direction[k] == "T" & k == 1) {
      #  df$direction[k] <- "T"
      #}
      
      # identify transfers
      if (df$station[k] != df$station[j] & df$line[k] != df$line[j]) {
        
        # identify an 'implicit transfer (e.g. transfer 120->120 from 1 to 2/3)
        if (df$line[j] != "") {
          df <- df %>% add_row(itinerary_id = df$itinerary_id[j], station = df$station[j],
                               line = df$line[k], leg = l, weight = 0, .after = k)
        }
        
        # identify an 'explicit' transfer (e.g. transfer R11->629 from N/R to 4)
        else {
          df$line[j] <- df$line[k]
          df$leg[j] <- df$leg[k]
        }
        
        # make note of transfer events
        df$event[j] <- "start_transfer"
        df$event[j+1] <- "end_transfer"
        
        
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
  # itinerary <- itinerary %>%
  #   left_join((stops %>% select(stop_id, stop_name)), by=c("station" = "stop_id"))
  # 
  return(itinerary)
}

format_itinerary <- function(paths, graph, src, dest, stops = NULL, map = NULL, attributeNames=NULL){
  path_tibble <- combine_paths_to_tibble(paths,graph, attributeNames)
  if (is.null(path_tibble)) {
    warning(paste('No path between', src, dest))
    return(NULL)
  }
  
  if (!is.null(map)){
      path_tibble <- path_tibble %>% mutate(nxt_station = lead(station)) %>% 
      left_join(map, by = c('station'='stop_id_u', 'nxt_station'='nxt_stop_id_u')) %>% 
      mutate(nxt_stop_id= lag(nxt_stop_id)) %>% 
      group_by(itinerary_id) %>%  mutate(station2 = ifelse(stop_id == nxt_stop_id, stop_id, NA),
                                         station2 = ifelse(is.na(stop_id), nxt_stop_id, station2),
                                         station2 = ifelse(is.na(nxt_stop_id), stop_id, station2),
                                         station2 = ifelse(is.na(station2) & nchar(stop_id) == 3 & nchar(nxt_stop_id) == 4, nxt_stop_id, station2),
                                         station2 = ifelse(is.na(station2) & nchar(nxt_stop_id) == 3 & nchar(stop_id) == 4, stop_id, station2),
                                         station2 = ifelse(nchar(station2) == 3, paste0(station2, 'N'), station2))
    
    path_tibble[,'order'] <- 1:nrow(path_tibble)
    
    newrows <- list()
    
    for (i in 1:nrow(path_tibble)){
      row <- path_tibble[i,]
      if(is.na(row$station2))
      {
        path_tibble[i,'station2']<- path_tibble[i,'stop_id']
        j <- row
        j_attri <- edge.attributes(graph, get.edge.ids(graph, c(row$station,row$station)))
        n_j <- names(j)
        n_ja <- names(j_attri)
        n <- n_ja[n_ja%in%n_j]
        j[n] <- as.character(j_attri[n])
        j['station2'] <- row$nxt_stop_id
        j['order'] <- row$order-.5
        j['line'] <- 'T'
        newrows[[length(newrows)+1]] <- j
      }
    }
    path_tibble <- rbind(path_tibble, newrows) %>% arrange(order) %>% mutate(station = station2) %>% 
      select(-station2, -order, -stop_id, -nxt_stop_id, -nxt_station) %>%
      ungroup
  }
  # result <- path_tibble
  result <- format_itinerary_raw(greedy(path_tibble))
  if (is.null(stops)) result else left_join(result, stops[,c('stop_id','stop_name')], by = c('station' ='stop_id'))
 }

get_itinerary_directed <- function(graph, src, dest, k, stops = NULL, attributeNames = NULL){
  k_shortest_yen(graph, src, dest,k) %>% format_itinerary(graph, src, dest, stops, attributeNames)
}

get_itinerary<- function(graph, src, dest, k, stops = NULL, map = NULL, attributeNames = NULL){
  k_shortest_yen(graph, src, dest,k) %>%
    format_itinerary(graph, src, dest, stops, map, attributeNames)
}

get_multiple_edges <- function(source,neighbors, mode){
  edges <- reduce(neighbors, function(prev, cur) c(prev, source, cur))
  if (mode == 'out') c(source, edges) else c(edges, source)
}

merge_vertex <- function(graph, parent, children, mode){
  graph <- add_vertices(graph, nv = 1, name = parent)
  neighbor_list <- NULL
  for (child in children){
    neighbor <- neighbors(graph, child, mode)$name
    neighbor <- neighbor[! neighbor %in% neighbor_list]
    neighbor_list <- c(neighbor_list, neighbor)
    if (length(neighbor) == 0) next
    neighbor_edges <- E(graph, get_multiple_edges(child, neighbor,mode))
    new_edge_vertices <- get_multiple_edges(parent, neighbor, mode)
    graph <- add_edges(graph, new_edge_vertices, weight = neighbor_edges$weight, route_id = neighbor_edges$route_id)
  }
  delete.vertices(graph, children)
}

get_itinerary_complex <- function(graph, src, dest, k, stations, stops = NULL, map= NULL, attributeNames = NULL){
  old_graph <- graph
  src_children <- stations[stations[,'Complex ID'] == src,'GTFS Stop ID'][[1]]
  dest_children <- stations[stations[,'Complex ID'] == dest,'GTFS Stop ID'][[1]]

  src_name <- paste0('CID', src)
  dest_name <- paste0('CID', dest)
  graph <- merge_vertex(graph, src_name, src_children, 'out')
  graph <- merge_vertex(graph, dest_name, dest_children, 'in')
  
  paths <- k_shortest_yen(graph, src_name,dest_name,k) 
  paths <- lapply(paths,function(path){
      path_len <- length(path)
      for (src_child in src_children){
        if (are.connected(old_graph, src_child, path[2])){
          path[1] <- src_child
          break
        }
      }
      for (dest_child in dest_children){
        if (are.connected(old_graph, path[path_len-1], dest_child)){
          path[path_len] <- dest_child
          break
        }
      }
      path
    })
  paths %>% format_itinerary(old_graph, src, dest, stops,map, attributeNames)
}

