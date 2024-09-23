#' linregClass
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
#' 

linregClass <- setRefClass(
  "linregClass",
  fields = list(
    formula = "formula",
    data = "data.frame",
    X = "matrix",
    y = "character"
  ),
  methods = list(
    # constructor
    initialize = function(formula = NA, data = NA) {
      stopifnot(rlang::is_formula(formula))
      stopifnot(is.data.frame(data))
      .self$formula <- formula  
      .self$data <- data 
      .self$X <- model.matrix(formula, data)  
      .self$y <- all.vars(formula)[1]
    },  
    printMembers = function() {
      cat("Formula:", deparse(.self$formula), "\n")
      cat("Data:\n")
      print(head(.self$data))
      cat("Matrix X:\n")
      print(head(.self$X))
      cat("Dependent variable (y):", .self$y, "\n")
    },
    ordinaryLeastSquare = function() {}
  )
)