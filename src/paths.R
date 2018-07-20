path.order <- function(paths)
{
  if (is.character(paths)) paths <- path.asList(paths)
  path.order_raw(paths)
}

path.order_raw <- function(paths)
{
  stop_list <- paths %>% unlist %>% unique
  path_matrix <- as.data.frame(sapply(paths, function(x) match(stop_list,x)))
  len <- length(stop_list)
  stop_order <- c()
  for (i in 1:len){
    for (j in 1:len){
      found <- all(path_matrix[j,] == i, na.rm = T)
      if (found){
        stop_order[j] <- i
        path_matrix[j,] <- -len
        cols <-sapply(path_matrix,function(col) i %in% col)
        path_matrix[,cols] <- path_matrix[,cols]+1 
        break
      }
    }
  }
  stop_list[order(stop_order)]
}

path.asList <- function(pathStr_list, sep ='>') pathStr_list %>% sapply(function(x) strsplit(x,sep))


#
#p_ordered <- list()
#p_ordered[[1]] <- c('a','c','e')
#p_ordered[[2]] <- c('b','c')
#p_ordered[[3]] <- c('c','d')
#p_ordered[[4]] <- c('a','b','d','e')

#path.order(p_ordered)