#like separate but when you dont know exactly how many into columns you might have.
#for example, the mta's stations.txt coded the routes in the dayroute column
#e.g: 
# stop_name, routes
# station1, Q B
# station2, Q F C
# station3, A B C
#will be split into 
#stop_name, Q, B, F, C, A,
#station1,  1, 1, 0, 0, 0
#station2,  1, 0, 1, 1, 0
#station3,  0, 1, 0, 1, 1
# with this function call separate_unknown(table, 'routes')

separate_unknown <- function(data, column, sep=' '){
  indicators <- data[[column]] %>% strsplit(sep) %>% sapply(function(row) sapply(row, function(x)1))
  indicators <- bind_rows(!!!indicators) %>% mutate_all(funs(replace(.,is.na(.),0)))
  cbind(data,indicators)
}