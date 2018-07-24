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
        total_path <- list(c(rootPath[-i], spurPath))
        if (!total_path %in% B) B[length(B)+1] <- total_path
      }
    }
    if (length(B) == 0) break
    B <- sortPath(graph, B)
    A[k_i] <- B[1]
    B <- B[-1]
  }
  A
}

#extracts route_ids, and direction_id from path
extract_data <- function(graph,i,path){
  edges <- E(graph, path=path)
  
  direction <- edges$direction_id
  direction <- c(direction,direction[length(direction)])
  
  line <- c(edges$route_ids,'end')
  
  tibble(itinerary_id = i, station = path, line, direction)
}
#combines paths into a tibble
paths_to_tibble <- function(graph, paths) {
  paths.asTibble <- 1:length(paths) %>%
  lapply(function(i) extract_data(graph,i, paths[[i]])) %>%
  reduce(rbind) 
}

#wrapper around yen, processes yen's result so the itinery functon can interact with it
k_shortest_path <- function(graph, from, to, k) k_shortest.yen(graph, from, to, k) %>% paths_to_tibble(graph=graph)
  

#===================Test==========================#
edgeList <- tibble(from=character(), to=character(), weight = numeric())
edgeList[nrow(edgeList)+1,] <-list('c','d',3)
edgeList[nrow(edgeList)+1,] <-list('d','f',4)
edgeList[nrow(edgeList)+1,] <-list('f','h',1)
edgeList[nrow(edgeList)+1,] <-list('c','e',2)
edgeList[nrow(edgeList)+1,] <-list('e','d',1)
edgeList[nrow(edgeList)+1,] <-list('e','f',2)
edgeList[nrow(edgeList)+1,] <-list('e','g',3)
edgeList[nrow(edgeList)+1,] <-list('g','h',2)
edgeList[nrow(edgeList)+1,] <-list('f','g',2)
edgeList[nrow(edgeList)+1,] <-list('a','b',2)
edgeList[nrow(edgeList)+1,] <-list('c','c',4)

graph <- graph.data.frame(edgeList)

k_shortest.yen(graph, 'c','c',7) #expect error
k_shortest.yen(graph,'c','h',7)
