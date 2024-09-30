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
#' 

linreg <- setRefClass(
  "linreg",
  fields = list(
    formula = "formula",
    data = "data.frame",
    X = "matrix",
    y = "character",
    regressionCoeff = "matrix",
    fittedValues = "matrix",
    residuals = "matrix",
    dof = "numeric",
    residualVariance = "numeric",
    regressionCoefficientsVariance = "matrix",
    tValues = "matrix",
    cumulativeDistribution  = "matrix"
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
      .self$calculateRegressionCoeff()
      .self$calculateFittedValues()
      .self$calculateResiduals()
      .self$calculateDegreesOfFreedom()
      .self$calculateResidualVariance()
      .self$calculateRegressionCoefficientsVariance()
      .self$calculateTDistribution()
      .self$calculateCumulativeDistribution()

    },  
    printMembers = function() {
      cat("Matrix X:\n")
      base::print(.self$cumulativeDistribution)
    },
    calculateRegressionCoeff = function() {
      transposeX = t(.self$X) 
      X_inverse <- solve(transposeX %*% .self$X)
      .self$regressionCoeff <- X_inverse %*% transposeX %*% .self$data[[.self$y ]]
    },
    calculateFittedValues = function() {
      .self$fittedValues <- .self$X %*% regressionCoeff
    },
    calculateResiduals= function() {
      .self$residuals <- .self$data[[.self$y]] - fittedValues
    },
    calculateDegreesOfFreedom = function(){
      n_observations <- nrow(.self$data)
      p_parameters<- ncol(.self$X) - 1  # -1 for the intercept
      .self$dof <- n_observations - p_parameters
    },
    calculateResidualVariance = function(){
      .self$residualVariance <- as.numeric(t(.self$residuals) %*% .self$residuals / .self$dof)
    },
    calculateRegressionCoefficientsVariance = function(){
      transposeX = t(.self$X) 
      X_inverse <- solve(transposeX %*% .self$X)
      .self$regressionCoefficientsVariance <- .self$residualVariance * solve(transposeX %*% .self$X)
    },
    calculateTDistribution = function(){
      # .self$tValues <- regressionCoeff %*% solve(expm::sqrtm(.self$regressionCoefficientsVariance)) %*% regressionCoeff
      standardErrors <- sqrt(diag(.self$regressionCoefficientsVariance))
      .self$tValues <- .self$regressionCoeff / standardErrors
    },
    calculateCumulativeDistribution = function(){
        .self$cumulativeDistribution <- pt(.self$tValues, .self$dof)
    },
    print = function() {
      cat("linreg(formula =", deparse(self$formula), ", data =", deparse(substitute(self$data)), ")\n")
      cat("\nCoefficients:\n")
      for (name in names(.self$regressionCoeff)) {
        cat(sprintf("%-15s", name))
      }
      cat("\n")
      for (coef in .self$regressionCoeff) {
        cat(sprintf("%-15.4f", coef))
      }
      cat("\n")
    },
    plot = function() {
      ggplot() +
        geom_point(aes(x = .self$fittedValues, y = .self$residuals, color = "red"), 
                   size = 3, 
                   alpha = 0.7, 
                   shape = 1, 
                   stroke = 1.5, 
                   fill = NA) + 
        labs(title = "Residuals vs Fitted",
             x = "Fitted values",
             y = "Residuals") +  
        theme_minimal() 
      
      standardizedResiduals <- abs(.self$residuals / .self$residualVariance ** 0.5) ** 0.5
      ggplot() +
        geom_point(aes(x = .self$fittedValues, y =standardizedResiduals, color = "red"), 
                   size = 3, 
                   alpha = 0.7, 
                   shape = 1, 
                   stroke = 1.5, 
                   fill = NA) + 
        labs(title = "Scale-Location",
             x = "Fitted values",
             y = expression(sqrt(abs("Standardized Residuals")))) +  
        theme_minimal() 
    },
    resid = function() {
      return(.self$residuals)
    },
    pred = function() {
      return(.self$fittedValues)
    },
    coef = function() {
      output <- as.vector(t(.self$regressionCoeff))
      names <- rownames(.self$regressionCoeff)
      names(output) <- names
      base::print(output)
    },
    summary = function() {
      se <- t(t(sqrt(diag(linreg_mod$regressionCoefficientsVariance))))
      names <- c("Estimate", "Std. Error", "t value", "p value")
      summary <- cbind(.self$regressionCoeff, se, .self$tValues, .self$cumulativeDistribution)
      colnames(summary) <- names
      base::print(summary)
      cat("\nResidual standard error: ",.self$residualVariance ," ")
      cat("on ", .self$dof, " degrees of freedom")
    }
  )
)

