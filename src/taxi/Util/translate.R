library(tibble)
#like join but for one/many to one relationships
#@dict, where to look for

translate <- function(table, dict, key, value, ...)
{
  args <- match.call(expand.dots = F)
  key <- toString(args$key)
  value <- parse.Vector(args$value)
  toTrans <- args$`...`
  argNames<-names(toTrans)
  ellipsis<- list()
  for (i in 1:length(toTrans))
  {
    newNames <- c()
    if (is.null(argNames[i])||is.na(argNames[i])||argNames[i] == '')
    {
      t.key<- toString(toTrans[i])
    }else
    {
      t.key <- argNames[i]
      newNames <- parse.Vector(toTrans[[i]])
    }
    
    for (j in 1:length(value))
    {
      newNames[j] <- ifelse(is.na(newNames[j])||is.null(newNames[j]), paste(t.key,value[j],sep='.'), newNames[j])
    }
    ellipsis[[i]] <-  c(t.key,newNames)
  } 
  
  translate_raw(table,dict,key,value, ellipsis)
}

getColumnAsVector <- function(table, columnName)
{
  column <- table[,columnName]
  if (is.tibble(table)) return(column[[1]])
  column
}

#raw
#like join but for one to one relationships
translate_raw <- function(table, dict, dict_key, values, args)
{
  dRows <- getColumnAsVector(dict,dict_key)
  for (arg in args)
  {
    key <- arg[1]
    newNames <- arg[-1]
    tRows <- getColumnAsVector(table,key)
    indices <- match(tRows, dRows)
    table[,newNames] <- dict[indices,values]
  }
  table
}


#parses language in to vector
parse.Vector <- function(v)
{
  v <- sapply(v, function(x) toString(x))
  if (length(v) == 1) return (v) else return(v[-1])
}


spread_key <- function(x, column, dummyName='dummy',na = 0)
{
  names.og <- names(x)
  args <- list('.data' = x)
  args[dummyName] <- 1
  x <- do.call(mutate, args)
  x <- do.call(spread, list(data = x, key = column, value = dummyName))
  names.new <- names(x)
  names.new <- names.new[!names.new %in% names.og]
  x %>% mutate_at(.vars = names.new, function(x) ifelse(is.na(x), 0,1))
}

spread_key_mult <- function(x, ..., dummyName = 'dummy')
{
  for (c in list(...))
  {
    x <- spread_key(x, c, dummyName)
  }
  x
}
