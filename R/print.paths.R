#####################################################
# Print method for paths objects
#####################################################
#'
#' @param x a fitted model object returned by the \code{\link{paths}} function.
#' @param digits minimal number of significant digits printed.
#' @rdname paths
#' @export
print.paths <- function(x, digits = 3, ...) {
  cat("\n")

  cat("Causal Paths Analysis \n\n")

  # Print function calll
  cat("Call: ")
  print(x$call)
  cat("\n")

  # Print model variables
  cat("Treatment:", x$varnames$a, "\n")
  cat("Outcome:", x$varnames$y, "\n\n")

  cat("Outcome model: ")
  print(x$formulas[[1]])
  cat("\n")

  K <- length(x$varnames$m)

  for(i in 1:K) {
    cat("Mediator", i, ": ", paste0(x$varnames$m[[i]], collapse = " + "), "\n")
  }
  cat("\n")

  # Print effect estimates
  cat("Causal Paths Estimates: \n\n")
  cat("Pure Imputation Estimator: \n")
  print(x$pure[, -1], digits = digits, ...)
  cat("\n")
  if (!is.null(x$ps_formula)){
    cat("Imputation-based Weighting Estimator: \n")
    print(x$hybrid[, -1], digits = digits, ...)
  }

  cat("\n")

  invisible(x)
}


#####################################################
# Summary method for paths objects
#####################################################
#' Summarizing Output from Causal Paths Analysis
#'
#' Function to report results from causal paths analysis. Report point estimates
#' and standard errors for the total effect, direct effect, and each individual
#' indirect effect, separately for Type I and Type II decompositions.
#'
#' @param object an object of class \code{paths} returned by the
#'   \code{\link{paths}} function.
#' @param ... additional arguments to be passed to \code{printCoefmat} for the
#' \code{print} method
#'
#' @return An object of class \code{summary.paths}, which is a list containing
#'   the \code{call}, \code{varnames}, \code{formulas}, \code{classes},
#'   \code{args}, \code{ps_formula}, \code{ps_class}, \code{ps_args},
#'   \code{nboot}, \code{conf_level} components from the \code{paths} object,
#'   plus \describe{ \item{nobs}{number of observations in \code{data}} \item{estimates}{a
#'   list containing four matrices, corresponding to effect estimates obtained
#'   using the pure imputation estimator and the imputation-based weighting
#'   estimator, each with Type I and Type II decompositions. Each matrix
#'   contains the point estimates, standard errors, and confidence intervals of
#'   the total effect, direct effect, and each individual indirect effect
#'   for the corresponding decomposition. The elements in each matrix
#'   are extracted from the \code{paths} object.}}
#'
#' @details \code{print.summary.paths} tries to smartly format the point
#'   estimates and confidence intervals, and provides 'significance stars'
#'   through the \code{\link[stats]{printCoefmat}} function.
#'
#'   It also prints out the names of the treatment, outcome, mediator variables as well
#'   as pretreatment covariates, which are extracted from the \code{formulas} argument of the
#'   call to \code{paths} so that users can verify if the model formulas have been
#'   correctly specified.
#'
#' @example inst/examples/summary.paths-example.R
#'
#' @aliases print.summary.paths
#' @export
summary.paths <- function(object, ...){

  call <- object$call
  varnames <- object$varnames
  formulas <- object$formulas
  classes <- object$classes
  args <- object$args
  ps_formula <- object$ps_formula
  ps_class <- object$ps_class
  ps_args <- object$args
  nobs <- nrow(object$data)
  nboot <- object$nboot
  conf_level <- object$conf_level
  K <- length(varnames$m)

  clp <- 100 * conf_level

  # Extract 4 separate tables
  estimates_pure_t1 <- object$pure[object$pure$decomposition == "Type I", - c(1,2,3)]
  estimates_pure_t2 <- object$pure[object$pure$decomposition == "Type II", - c(1,2,3)]
  estimates_hybrid_t1 <- object$hybrid[object$hybrid$decomposition == "Type I", - c(1,2,3)]
  estimates_hybrid_t2 <- object$hybrid[object$hybrid$decomposition == "Type II", - c(1,2,3)]

  rownames(estimates_pure_t1) <-
    rownames(estimates_pure_t2) <-
    rownames(estimates_hybrid_t1) <-
    rownames(estimates_hybrid_t2) <-

    c("Direct Effect: A -> Y",
      paste0("Indirect Effect: A -> ", "M", K, " -> Y"),
      paste0("Indirect Effect: A -> ", "M", (K-1):1, " ~> Y"),
      "Total Effect: A ~> Y")

  colnames(estimates_pure_t1) <-
    colnames(estimates_pure_t2) <-
    colnames(estimates_hybrid_t1) <-
    colnames(estimates_hybrid_t2) <-
    c("Estimate",
      "Std. Err.",
      paste(clp, "% CI Lower", sep=""),
      paste(clp, "% CI Upper", sep=""),
      "P-value")

  estimates <- if(!is.null(object$ps_formula)) {
    list(pure_t1 = estimates_pure_t1,
         pure_t2 = estimates_pure_t2,
         hybrid_t1 = estimates_hybrid_t1,
         hybrid_t2 = estimates_hybrid_t2)
  } else {
    list(pure_t1 = estimates_pure_t1,
         pure_t2 = estimates_pure_t2)
  }

  out <- list(call = call,
              varnames = varnames,
              formulas = formulas,
              classes = classes,
              args = args,
              ps_formula = ps_formula,
              ps_class = ps_class,
              ps_args = ps_args,
              nobs = nobs,
              nboot = nboot,
              conf_level = conf_level,
              estimates = estimates)

  class(out) <- "summary.paths"

  out
}

#' @param x an object of class \code{summary.paths}
#' @rdname summary.paths
#' @export
print.summary.paths <- function(x, ...) {

  clp <- 100 * x$conf_level

  cat("\n")

  cat("Causal Paths Analysis \n\n")

  # Print function calll
  cat("Call: ")
  print(x$call)
  cat("\n")

  # Print model variables
  cat("Treatment:", x$varnames$a, "\n")
  cat("Outcome:", x$varnames$y, "\n\n")

  cat("Outcome model: ")
  print(x$formulas[[1]])
  cat("\n")

  K <- length(x$varnames$m)

  for(i in 1:K) {
    cat("Mediator", i, ": ", paste0(x$varnames$m[[i]], collapse = " + "), "\n")
  }
  cat("\n")

  # Print output table

  # Print effect estimates
  cat("Causal Paths Estimates: \n\n")

  cat("Pure Imputation Estimator: \n")
  # Use the printCoefmat() function to conveniently generate
  # summary table
  # Note the use of test statistic-like format
  # (through tst.ind and dig.test) for Estimate and CIs columns
  cat("Type 1 Decomposition: \n")
  printCoefmat(x$estimates$pure_t1,
               digits = 2,
               P.values = TRUE,
               tst.ind = 1:3,
               dig.tst = 3,
               has.Pvalue = TRUE,
               ...)

  cat("\n")
  cat("Type 2 Decomposition: \n")
  printCoefmat(x$estimates$pure_t2,
               digits = 2,
               P.values = TRUE,
               tst.ind = 1:3,
               dig.tst = 3,
               has.Pvalue = TRUE,
               ...)
  cat("\n\n")
  if (!is.null(x$ps_formula)){
    cat("Imputation-based Weighting Estimator: \n")
    cat("Type 1 Decomposition: \n")
    printCoefmat(x$estimates$hybrid_t1,
                 digits = 2,
                 P.values = TRUE,
                 tst.ind = 1:3,
                 dig.tst = 3,
                 has.Pvalue = TRUE,
                 ...)
    cat("\n")
    cat("Type 2 Decomposition: \n")
    printCoefmat(x$estimates$hybrid_t2,
                 digits = 2,
                 P.values = TRUE,
                 tst.ind = 1:3,
                 dig.tst = 3,
                 has.Pvalue = TRUE,
                 ...)
    cat("\n\n")
  }
  cat("Sample size:", x$nobs,"\n\n")
  cat("Number of bootstrap simulations:", x$nboot,"\n\n")

  invisible(x)
}
