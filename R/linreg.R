#' linreg
#'
#' @description This class implements different linear regression algorithms.
#' @param formula A list representing linear regression formula.
#' @param data The imput data for the model.
#' @param method This parameter determines the calculation method.
#' @param model This parameter determines which model you choose.
#' @return A object which can return coefficients, residuals, degree of freedom and so on.
#' @import ggplot2
#' @import png
#' @import grid
#' @importFrom methods new
#' @export linreg
#' @exportClass linreg

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
    initialize = function(formula = NA, data = NA, method = NA, model = NA) {
      stopifnot(rlang::is_formula(formula))
      stopifnot(is.data.frame(data))
      stopifnot(is.character(model))
      .self$formula <- formula  
      .self$data <- data 
      .self$X <- model.matrix(formula, data)  
      .self$y <- all.vars(formula)[1]
      
      
      
      if(model == "ridge"){
        .self$call <- paste("ridge(formula = ", deparse(formula), ", data = ", deparse(substitute(data)), ")", sep = "")
        
        # I do not plan calculate the coefficients here, 'cause it the name of initialize function is linreg
        # which means i have to call the function manually.
      }
      else if(model == "linear"){
        .self$call <- paste("linreg(formula = ", deparse(formula), ", data = ", deparse(substitute(data)), ")", sep = "")
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
      }
      else{
        stop("You can only choose ridge or linear model!")
      }
      
    },
    getTheme = function(){
      return(theme(panel.background = element_rect(fill = rgb(172/255, 234/255, 233/255), color = NA),
            plot.background = element_rect(fill = rgb(172/255, 234/255, 233/255), color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()))
    }, 
    getLogo = function(){
      # the plot() example in vigenttes cannot access logo.png via this path
      # so i used try catch to fix that
      logo <- tryCatch({
        readPNG("inst/resources/liulogo.png")
      },error = function(e){
        readPNG("../inst/resources/liulogo.png")
      })
      # logo <- readPNG("resources/liulogo.png")
      logoRaster <- rasterGrob(logo, width = unit(0.5, "npc"), height = unit(0.5, "npc"))
      return(logoRaster)
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

    #' @description Function print() implements printing the coefficients matrix of regression.
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
    #' @description Function plot() implements plots named "Residuals vs Fitted" and "Scale-Location".
    #' @param which "which" determines which plot would be demostrated, when which is 1, "Residuals vs Fitted" demostrated
    #' and when which is 3, "Scale-Location" demostrated.
    plot = function(which = NA) {
      stopifnot(is.numeric(which))
      if(which == 1){
        data_p1 <- data.frame(x = .self$fittedValues, y = .self$residuals)
        ggplot(data_p1, aes(x = x, y = y)) +
        annotation_custom(.self$getLogo())  +
        geom_point(color = "blue", size = 3) + 
        stat_summary(fun = median, geom = "line", color = "red", linewidth = 1.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "green", linewidth = 0.5) +
        labs(title = "Residuals vs Fitted", x = "Fitted values", y =  "Residuals") +
        theme_minimal() + 
        .self$getTheme()+
        coord_cartesian(clip = "off")
      }
      else if(which == 3){
        standardizedResiduals <- abs(.self$residuals / .self$residualVariance ** 0.5) ** 0.5
        data_p2 <- data.frame(x = .self$fittedValues, y = standardizedResiduals)
        ggplot(data_p2, aes(x = x, y = y)) +
        annotation_custom(.self$getLogo())  +
        geom_point(color = "blue", size = 3) + 
        stat_summary(fun = median, geom = "line", color = "red", linewidth = 1.5) +
        labs(title = "Scale-Location", x = "Fitted values", y =  expression(sqrt(abs("Standardized Residuals")))) +
        theme_minimal() + 
        .self$getTheme() +
        coord_cartesian(clip = "off")
      }
      else{
        cat("input error\n")
      }

    },
    #' @description Function resid() returns the residuals
    #' @return The residual matrix of regression
    resid = function() {
      return(.self$residuals)
    },

    #' @description Function pred() returns the predicted value of y
    #' @param newData Calculate output based on new data
    #' @return The predicted value of y
    pred = function() {
      return(fittedValues)
    },

    #' @description Function coef() returns the coefficients matrix
    #' @return The residual matrix of regression
    coef = function() {
      output <- as.vector(t(.self$regressionCoeff))
      names <- rownames(.self$regressionCoeff)
      names(output) <- names
      # aim to pass the unit test, it is necessary to call base::print()
      base::print(output)
    },
    #' @description Function summary() prints a similar printout as printed for lm objects, 
    #' presenting the coeffcients with their standard error, t-value and p-value as well as the estimate of residual 
    #' variance and the degrees of freedom in the model.
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
    },
    #' @description This function is implemented for initializing the ridge regression model 
    #' @param lambda Hyper parameter
    ridgereg = function(lambda, method, regularization = FALSE){
      if(!is.numeric(lambda)){
        lambda <- as.numeric(lambda)
      }
      # method = 1, we use linear algebra
      # method = 2, we use QR decomposition
      stopifnot(is.numeric(lambda))
      stopifnot(is.logical(regularization))
      lambda_I <- diag(lambda, ncol(.self$X))
      unsolved_matrix <- t(.self$X) %*%.self$X + lambda_I
      
      if(regularization){
        regularize()
      }
     
      if(method == 1){
        if(det(unsolved_matrix) != 0){
          .self$regressionCoeff <- solve(unsolved_matrix) %*% t(.self$X) %*% .self$data[[.self$y]]
        }
        else{
          stop("the matrix is singular!")
        }
      }
      else if(method == 2){
        tilde_X <- rbind(.self$X, lambda**0.5 * diag(ncol(.self$X)))  
        tilde_y <- c(.self$data[[.self$y]], rep(0, ncol(.self$X)))  
        qr_decomp <- qr(tilde_X)
        .self$regressionCoeff <- as.matrix(qr.coef(qr_decomp, tilde_y))
      }
      else
        stop("no such method.")
      
      .self$calculateFittedValues()
    },
    regularize = function(){
      .self$X <- .self$X[, -1]
      .self$X <- (.self$X - mean(.self$X)) / sd(.self$X)
      .self$X <- cbind(1, .self$X)
    }
  )
)



#  mod_object <- linreg(Petal.Length~Species, data = iris, model = "ridge")
# mod_object$ridgereg(lambda = 0.2, method = 2)
# mod_object$print()

