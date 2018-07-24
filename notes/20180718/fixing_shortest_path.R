
# Determine if a given path variant has already been seen
contains.path <- function(variants, variant){
  return( any( unlist( lapply( variants, function(x){ identical(unlist(x$variant$vert),unlist(variant)) } ) ) ) )
}

# Choose shortest path variant
select.shortest.path <- function(variants){
  return( which.min( unlist( lapply( variants, function(x){x$variants$dist} ) ) ) )
}

# Return all variants of a given path
calculate.variants <- function(variants, variant, from, to, gmode){
  # Take graph from current path
  g <- variant$g
  
  # Iterate through edges, removing one each iterations
  for (j in unlist(variant$path)){
    newgraph <- delete.edges(g, j) # Remove edge
    sp <- get.shortest.paths(newgraph,from,to, output='both', mode=gmode) # Calculate shortest path
    spd <- shortest.paths(newgraph,from,to,mode=gmode) # Calculate length
    if (spd != Inf){ # The the path is found
      if (!contains.path(variants, sp$vpath)) # Add to list, unless it already contains the same path
      {
        variants[[length(variants)+1]] <- list(g=newgraph, variants=list(path=sp$epath, vert=sp$vpath, dist=spd))
      }
    }
  }
  return(variants)
}

######################
# Main K Path Function
######################

k.shortest.paths <- function(graph, from, to, k, gmode){
  # First shortest path
  k0 <- get.shortest.paths(graph,from,to, output='both', mode=gmode)
  
  # Number of currently found shortest paths
  kk <- 1
  
  # List of alternatives
  variants <- list()
  
  # Shortest variants
  shortest.variants <- list(list(g=graph, path=k0$epath, vert=k0$vpath, dist=shortest.paths(graph,from,to,mode=gmode)))
  
  # Until k shortest paths are found
  while(kk<k){
    # Take last found shortest path
    last.variant <- shortest.variants[[length(shortest.variants)]]              
    
    # Calculate all alternatives
    variants <- calculate.variants(variants, last.variant, from, to, gmode)
    
    # Find shortest alternative
    sp <- select.shortest.path(variants)
    
    # Add to list, increase kk, remove shortest path from list of alternatives
    shortest.variants[[length(shortest.variants)+1]] <- list(g=variants[[sp]]$g, path=variants[[sp]]$variants$path, vert=variants[[sp]]$variants$vert, dist=variants[[sp]]$variants$dist)
    kk <- kk+1
    variants <- variants[-sp]
  }
  return(shortest.variants)
}


 k.shortest.paths(graph, 'c','h',5,'out') %>% sapply(function(x) x$path)
