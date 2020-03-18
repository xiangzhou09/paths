#####################################################
# Print method for paths objects
#####################################################

#' @param x a fitted model object returned by the \code{\link{paths}} function.
#' @rdname paths
#' @export
print.paths <- function(x, ...) {
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

  K <- length(x$varnames) - 3

  for(i in 1:K) {
    cat("Mediator ", i, ": ", paste0(x$varnames[[i+2]], collapse = " + "), "\n")
  }
  cat("\n")

  # Print effect estimates
  cat("Causal Paths Estimates: \n\n")
  cat("Pure Imputation Estimator: \n")
  print(x$pure)
  cat("\n")
  if (!is.null(x$ps_formula)){
    cat("Imputation-based Weighting Estimator: \n")
    print(x$hybrid)
  }

  cat("\n")

  invisible(x)
}

