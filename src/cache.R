library(readr)

init_cache <- function(cache_path){
  if (!file.exists(cache_path)) {
    dir.create(cache_path,recursive = T)
    print(paste('Creating cache folder at', normalizePath(cache_path, winslash = '/')))
  }
  sub_path <- function(name) paste(cache_path, name, sep ='/')
  
  cache <<- function(name, creator=NULL, ..., recompute = F){
    cache_sub_path <- sub_path(name)
    if (is.null(creator)) creator <- get(paste0('create_',name))
    if (file.exists(cache_sub_path) & !recompute) {
      print(paste('loading from cache: ', name))
      load(cache_sub_path,envir = .GlobalEnv)
    }
    else{
      data <- creator(...)
      assign(name, data, envir=.GlobalEnv)
      do.call(save, args= list(file = cache_sub_path, ... = name))
    }
  }
  
  cache.read <<- function(name, path, ...) cache(name, read_csv, path, ...)
  clean_cache <<- function(name){
    cache_sub_path <- sub_path(name)
    file.remove(name)
  } 
}

#init_cache('../cache/graph_itinerary_multiple')
#cache.read('stop_times', '../data/google_transit_subway_static/stop_times.txt', col_types =cols('arrival_time'='c', 'departure_time' ='c'))
