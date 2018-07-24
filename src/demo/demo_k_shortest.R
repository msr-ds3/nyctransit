source('../k_shortest.R')
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
k_shortest.yen(graph,'c','h',1)
