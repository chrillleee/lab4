#' linreg
#'
#' @description This function implements different linjear regression algorithms.
#' @param formula A list representing the adjacency list of the graph.
#' @param data The starting node for the algorithm.
#' @return A list containing the shortest paths from the source to all other nodes.
#' @examples
#' linreg(1, 2)
#' 
#' @references
#' Wikipedia: \href{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}{Dijkstra's algorithm}
#' @export

linreg <-function(formula, data){
  return(linregClass$new(formula, data))
}