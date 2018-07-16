library(readr)
removeEmptyColumns <- function(table){
  logical_column_indices <- !sapply(table, function(x)all(is.na(x)))
  table[,logical_column_indices]
}

computeColTypes <- function(...)
{
  types <- list(...)
  x <- unlist(types)
  if (is.null(x)) return (NULL)
  y <- x %>% names() %>% gsub(pattern='[0-9]*$', replacement = '')
  names(y) <- x
  y
}

determineProblemCause<- function(table)
{
  problem <- problems(table) %>% select(col, expected) %>% distinct
  problem.c <- problem[grep(problem$expected, pattern = 'an .*'), 'col'][[1]]
  problem.d <- problem[grep(problem$expected, pattern = 'no trailing characters'), 'col'][[1]]
  problem.d <- problem.d[!problem.d%in% problem.c]
  computeColTypes(c = problem.c, d = problem.d)
}

read <- function(name, ..., removeEmpty = T, resolveExceptions = F)
{
  colTypes <- computeColTypes(...)
  if (resolveExceptions)
  {
    table <- read_raw(name, T, colTypes)
    causes <-determineProblemCause(table)
    if (length(causes) != 0)
    {
      table <- read_raw(name,F, colTypes, causes)
    }
  } else 
  {
    table <- read_raw(name, F, colTypes)
  }
  names(table) <- gsub(' ', '_', names(table))
  if (removeEmpty) table <- removeEmptyColumns(table)
  return(table)
}

read_raw <- function(name, silent, ...)
{
  types <- do.call(cols,as.list(c(..., '.default' = '?')))
  if (silent)
    return (suppressWarnings(read_csv(name, col_types = types)))
  read_csv(name, col_types = types)
}

pathname <- function(folder, name) paste(folder, name, sep='/') %>% gsub(pattern = '(?!^\\..*)\\.', replacement = '_', perl =T) %>%  paste('csv', sep ='.') 
rdataname <- function(folder) paste(basename(folder), 'rdata',sep='.') %>% paste(folder, ., sep='/')

save.mult <- function(folder = './', ..., create_rdata = T)
{
  names <- substitute(list(...))[-1] %>% sapply(function(x) toString(x))
  data <- names %>% sapply(function(x) get(x))
  save.mult_raw(folder, names, data, create_rdata)
}

save.mult_raw <- function(folder, names, data, create_rdata){
  if (!dir.exists(folder)) dir.create(folder)
  
  for (i in 1:length(names))
  {
    write_csv(data[[i]],pathname(folder, names[i]))
  }
  if (create_rdata)
  {
    save(file = rdataname(folder),list = names)
  }
}

load.mult <- function(folder, ...){
  fileNames <- list(...)
  if (length(fileNames) == 0){
    rdata <- rdataname(folder)
    
    if (file.exists(rdata))
    {
      load(rdata, envir = .GlobalEnv)
    }
    else {
      paths <- list.files(folder) %>% gsub(pattern='.csv', replacement='')
      load.mult_raw(folder, paths, paths)
    }
  }else
  {
    newNames <- names(fileNames)
    if (is.null(newNames)) newNames <- rep('',length(fileNames))
    emptyIndices <- is.na(newNames)| newNames == ''
    newNames[emptyIndices] <- fileNames[emptyIndices]
    load.mult_raw(folder, fileNames, newNames)
  }
}

load.mult_raw <- function(folder, names, newNames)
{
  for (i in 1:length(names))
  {
    path <- pathname(folder, names[i])
    assign(x=newNames[[i]], value = read(path, resolveExceptions = T), envir = .GlobalEnv)
  }
}
