#' Dijkstras algorithm
#' 
#' find the the shortest path
#' 
#' The algorithm takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph.
#' 
#' @param graph a data.frame with three variables (v1, v2 and w) that contains the edges of the graph (from v1 to v2) with the weight of the edge (w)
#' @param init_node  a numeric scalar that exist in the graph
#' @return the shortest path to every other node from the starting node as a vector.
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' 
#' @export
#' @examples 
#' dijkstra(wiki_graph, 1)
#' 
dijkstra <-
function(graph, init_node){
  assert_args <- is.data.frame(graph) && all(names(graph) == c("v1", "v2", "w")) && any(init_node == graph[1]) && is.numeric(init_node) && length(init_node) == 1
  stopifnot(assert_args)
  # Assert arguments
  node_count <- nlevels(factor(graph[, 1])) 
  #number of all nodes
  list_spath <- array(rep(Inf, node_count)) 
  #initialize an array, stores the shortet weight to each node that you will return
  visited <- array(rep(0), node_count)
  #initialize an array, stores visited nodes
  for (i in c(1:(node_count - 1))) {
    #loop [node_count - 1] times for shortest path takes [n - 1] edges
    temp_gph <- graph[which(graph[, 1] == init_node), ]
    # shorten the graph (unnecessary)
    if (list_spath[init_node] == Inf) list_spath[init_node] <- 0
    # set first node's distance to itself(0)
    for (j in temp_gph[, 2]) {
      if (list_spath[j] == Inf) {
        list_spath[j] <- temp_gph[which(temp_gph[,2] == j), 3] + list_spath[init_node]
      }  else
        list_spath[j] <- min(temp_gph[which(temp_gph[,2] == j), 3] + list_spath[init_node], list_spath[j])
    }
    #compare the weight to next stand node by node, then update it
    visited[i] <- init_node
    init_node <- max(which(list_spath == min(list_spath[-visited])))
    #update visited list and next node to go
  }
  dim(list_spath) <- NULL
  #seems testthat don't accept array but we are already here
  return(list_spath)
}
