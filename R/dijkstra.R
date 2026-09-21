#'Dijkstra's algorithm
#'
#'
#' This is a description
#'
#'
#'
#'@param graph A \code{data.frame} which includes 3 columns, \code{v1} \code{v2}
#'            and \code{w}. Where \code{v1} and \code{v2} are vector nodes with
#'            their corresponding weight in column \code{w}
#'
#'@param init_node The node where the search starts from
#'@return The cost to each node from the start node
#'@references https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm
#'@export
dijkstra <- function(graph, init_node){


  stopifnot(is.numeric(graph$v1), is.numeric(graph$v2), is.numeric(graph$w))
  stopifnot(!anyNA(graph[c("v1", "v2","w")]))
  stopifnot(length(graph$v1) == length(graph$v2), length(graph$v2) == length(graph$w))
  stopifnot(is.numeric(init_node), length(init_node) == 1, init_node %in% graph$v1)

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
