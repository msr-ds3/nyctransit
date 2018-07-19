library(tidyverse)
library(igraph)
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

graph <- graph.data.frame(edgeList)

shortestPath <- function(graph, from, to){
  path <- suppressWarnings(get.shortest.paths(graph, from, to, mode = 'out', output = 'both')) 
  names(unlist(path$vpath))
}

k_shortests.yen <- function(graph, from, to, k){
  A <- list(shortestPath(graph,from,to))
  B <- list()
  for (k_i in 2:k){
    for(i in 1:(length(A[[k_i-1]])-1)){
      spurNodes <- A[[k_i-1]][i]
      rootPath <- A[[k_i-1]][1:i]
      edgesToDelete <- list()
      for (p in A){
        if (all(p[1:i] == rootPath)) {
          edge <- paste(p[i], p[i+1], sep = '|')
          edgesToDelete <- append(edgesToDelete, edge)
        }
      }
      edgesToDelete <- unique(edgesToDelete)
      
      t_g <- graph
      for (edge in edgesToDelete){
        t_g <- delete.edges(t_g, edge)
      }
      spurPath <- shortestPath(t_g,spurNodes,to)
      if (length(spurPath) != 1){
        total_path <- c(rootPath[-i], spurPath)
        B[[length(B)+1]] <- total_path
        #print('fouuuuuund')
      }
      #print(paste('I:', i))
      #print(paste('A:',paste(A[[k_i-1]], collapse = ' ')))
      #print(paste('Edges TO Remove: ', paste(edgesToDelete, collapse = ' ')))
      #print(paste('spurNode', spurNodes))
      #print(paste('spurPath:', paste(spurPath, collapse = ' ')))
      #print(paste('RootPath:', paste(rootPath, collapse = ' ')))
      #print(paste('TotalPath: ', paste(total_path, collapse = ' ')))
      #print('========================')
    }
    if (length(B) == 0) break
    B <- sortPath(graph, B)
    while (B[1] %in% A) B <- B[-1]
    A[k_i] <- B[1]
    B <- B[-1]
   # print('***************************************')
  }
  A
}

distance <- function(graph, path) sum(E(graph, path=path)$weight)

sortPath <- function(graph,paths){
  order <- sapply(paths, function(x) distance(graph,x)) %>% order
  paths[order]
}

k_shortests.yen(graph, 'c','h',7)