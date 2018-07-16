source('../translate.R')
table <- data.frame(id=c(1,2,3,4,5), id2=c(5,4,3,2,1))
dict <- data.frame(id=c(1,2,3,4,5), value=c('a','b','c','d','e'), reverse=c('e','d','c','d','a'))

table.tb <- as.tibble(table)
dict.tb <- as.tibble(dict)


#testing with data frame
#auto name for both
translate(table, dict, id, c(value,reverse), id,id2)
#manual name
translate(table, dict, id, c(value,reverse), id=c(value,reverse),id2=c(value2,reverse2))
#auto + manual
translate(table, dict, id, c(value,reverse), id=c(value,reverse),id2)
#partial_manual + auto
translate(table, dict, id, c(value,reverse), id=c(value3),id2)


#testing with tibble
table <- table.tb
dict.tb <- dict.tb
#auto name for both
translate(table, dict, id, c(value,reverse), id,id2)
#manual name
translate(table, dict, id, c(value,reverse), id=c(value,reverse),id2=c(value2,reverse2))
#auto + manual
translate(table, dict, id, c(value,reverse), id=c(value,reverse),id2)
#partial_manual + auto
translate(table, dict, id, c(value,reverse), id=c(value3),id2)