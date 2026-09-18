
#UNFINISHED
dijkstra <- function(graph, init_node){

  dist <- graph$w
  source_idx <- which((graph$v1 == init_node))

  return(c(1:10))

}


wiki_graph <- data.frame(
  v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
  v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
  w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)

  )
start <- 3

dijkstra(wiki_graph, start)

