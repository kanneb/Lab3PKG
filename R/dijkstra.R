
dijkstra <- function(graph, init_node){

  stopifnot(is.numeric(graph$v1), is.numeric(graph$v2), is.numeric(graph$w))
  stopifnot(!anyNA(graph[c("v1", "v2","w")]))
  stopifnot(length(graph$v1) == length(graph$v2), length(graph$v2) == length(graph$w))

  weight <- graph$w
  cost <- c()
  queue <- unique(graph$v1)

  for (vertex in queue)
    cost[vertex] <- Inf

  cost[init_node] <- 0

  while (length(queue) != 0){
    #get least cost node in queue
    curr_node <- queue[which.min(cost[queue])]

    for(node_idx in which(graph$v1 == curr_node)){

      destination <- graph$v2[node_idx]
      new_cost <- weight[node_idx] + cost[curr_node]

      if(cost[destination] > new_cost){
        cost[destination] <- new_cost
      }
    }
    queue <- queue[!queue %in% curr_node] #remove checked node from queue

  }

  return(cost)

}


wiki_graph <- data.frame(
  v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
  v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
  w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)

  )
start <- 1

print(dijkstra(wiki_graph, start))

