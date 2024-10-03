#' linreg
#'
#' @description This function implements different linear regression algorithms.
#' @param formula A list representing linear regression formula.
#' @param data The imput data for the model.
#' @return A object which can return coefficients, residuals, degree of freedom and so on.
#' @examples
#' object <- linreg(formula = Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
#' object$print()
#' object$summary()
#' object$coef()
#' object$pred()
#' object$plot()
#' object$resid()
#' 

#' @references
#' Wikipedia: \href{"https://en.wikipedia.org/wiki/Linear_regression"}{Linear Regression}
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
    cumulativeDistribution  = "matrix",
    residualStandardError = "numeric",
    call = "character",
    formula_name = "character",
    data_name = "character"
  ),
  methods = list(
    # constructor
    initialize = function(formula = NA, data = NA, method = NA) {
      stopifnot(rlang::is_formula(formula))
      stopifnot(is.data.frame(data))
      .self$formula <- formula  
      .self$data <- data 
      .self$X <- model.matrix(formula, data)  
      .self$y <- all.vars(formula)[1]
      .self$call <- paste("linreg(formula = ", deparse(formula), ", data = ", deparse(substitute(data)), ")", sep = "")
      # .self$formula_name <-  deparse(formula)
      # .self$data_name <- deparse(substitute(data))
      # 1.2.2
      if(is.character(method)){
        y_ <- .self$data[[.self$y]]
        qr_decop <- qr(.self$X)
        q_matrix <- qr.Q(qr_decop)
        r_matrix <- qr.R(qr_decop) 
        qt_y <- t(q_matrix) %*% y_
  
        .self$fittedValues <- q_matrix %*% qt_y
        .self$regressionCoeff <- solve(r_matrix) %*% qt_y
        .self$calculateDegreesOfFreedom()
        .self$residuals <- y_ - .self$fittedValues
        .self$calculateResidualVariance()
        .self$regressionCoefficientsVariance <- .self$residualVariance * solve(r_matrix) %*% t(solve(r_matrix))

        
       }
       else if(!is.na(method)){
         stop("incorrect parameter method")
       }
       else{
        .self$calculateRegressionCoeff()
        .self$calculateFittedValues()
        .self$calculateResiduals()
        .self$calculateDegreesOfFreedom()
        .self$calculateResidualVariance()
        .self$calculateRegressionCoefficientsVariance()
       }
       .self$calculateTDistribution()
       .self$calculateCumulativeDistribution()
       .self$residualStandardError <- caluculateResidualStandardError()
      
    },  
    printMembers = function() {
      cat("Matrix X:\n")
      base::print(.self$cumulativeDistribution)
    },
    calculateRegressionCoeff = function() {
      transposeX = t(.self$X) 
      X_inverse <- solve(transposeX %*% .self$X)
      .self$regressionCoeff <- X_inverse %*% transposeX %*% .self$data[[.self$y]]
    },
    calculateFittedValues = function() {
      .self$fittedValues <- .self$X %*% regressionCoeff
    },
    calculateResiduals= function() {
      .self$residuals <- .self$data[[.self$y]] - fittedValues
    },
    calculateDegreesOfFreedom = function(){
      n_observations <- nrow(.self$data)
      # p_parameters<- ncol(.self$X) - 1  # -1 for the intercept
      # in the test, the degree of freedom is 147, we might not need to minus the intercept
      p_parameters<- ncol(.self$X)
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
        .self$cumulativeDistribution <- 2 * (1 - pt(abs(.self$tValues), .self$dof))
    },
    caluculateResidualStandardError = function(){
      .self$residualStandardError <- .self$residualVariance ** 0.5
    },
    print = function() {
      cat("Call: \n")
      cat(.self$call)
      cat("\n \n Coefficients:\n")
      output <- as.vector(t(.self$regressionCoeff))
      names <- rownames(.self$regressionCoeff)
      for(name in names)
        cat(sprintf("%15s", name))
      
      cat("\n")
      for(num in output)
        cat(sprintf("%15f", num))
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
      # aim to pass the unit test, it is necessary to call base::print()
      base::print(output)
    },
    summary = function() {
      se <- t(t(sqrt(diag(.self$regressionCoefficientsVariance))))
      summary <- cbind(.self$regressionCoeff, se, .self$tValues, .self$cumulativeDistribution)
      for(i in 1:nrow(summary)){
        cat(sprintf("%15s  ", rownames(summary)[i]))
        for(j in 1:ncol(summary)){
          if(j == ncol(summary)){
            p <- summary[i,j]
            if (p <= 0.0001){
               cat(sprintf("%15s", "****"))
            }
            else if (p <= 0.001) {
               cat(sprintf("%15s", "***"))
            }
            else if (p <= 0.01) {
               cat(sprintf("%15s", "**"))
            }
            else if (p <= 0.05) {
               cat(sprintf("%15s", "*"))
            }
            else{
              cat(sprintf("%15s", "."))
            }
          }
          else {
             cat(sprintf("%15f", summary[i,j]))
          }
        }
        cat("\n")
      }
      cat("Residual standard error: ", round(.self$residualStandardError, 4), " on ", .self$dof," degrees of freedom", sep = "")
    }
  )
)


#"Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*".
# Sepal\.Length         1\.775593       0\.064405      27\.569160              \.\\nResidual standard error: 0\.6465 on 147 degrees of freedom"