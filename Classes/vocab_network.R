
get_igraph <- function(matrix) {
  require(igraph)
  
  m <- matrix
  
  # change it to a Boolean matrix
  m[m>=2] <- 1
  # transform into a term-term adjacency matrix
  m <- m %*% t(m)
  
  # build a graph from matrix
  g <- graph.adjacency(m, weighted=T, mode = "undirected")
  
  # return
  return(g)
}


## format igraph ----




# plot(TermDocumentMatrix(Notes,
#                         control = list(
#                           dictionary = c(names(found))
#                         )
#       ), terms = names(found[found>3]), corThreshold = 0.7, weighting = F)
