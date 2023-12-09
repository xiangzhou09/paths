#####################################################
# Sensitivity Analysis
#####################################################
#'
#' Sensitivity Analysis for Unobserved Confounding on Path-Specific Causal Effects
#'
#' \code{sens} implements a set of bias formulas detailed in Zhou and Yamamoto (2020) for assessing
#' the sensitivity of estimated path-specific effects to an unobserved confounder \eqn{U} of a mediator-outcome
#' relationship. The user provides a fitted \code{paths} object, the mediator whose relationship
#' with the outcome is potentially confounded, the estimand whose sensitivity to unobserved
#' confounding is being investigated, type of estimator, type of decomposition, and possible values of
#' the \eqn{\gamma} and \eqn{\eta} parameters.
#'
#' @param object a fitted model object returned by the \code{\link{paths}} function.
#'
#' @param confounded a character string indicating the mediator whose relationship with the outcome
#'  is potentially confounded. One of \{\code{"M1", "M2", \ldots}\}.
#'
#' @param estimand a character string indicating the estimand whose sensitivity to unobserved
#'  confounding is being investigated. One of \{\code{"M1", "M2", \ldots}\} or \code{"direct"}.
#'
#' @param estimator type of estimator, the pure imputation estimator (\code{"pure"}) or the imputation-based
#' weighting estimator (\code{"hybrid"}).
#'
#' @param decomp type of decomposition, \code{"Type I"} or \code{"Type II"}.
#'
#' @param gamma_values potential values of the \eqn{\gamma} parameter, which denotes the average effect of
#'   the unobserved confounder \eqn{U} on the outcome given pretreatment covariates \eqn{X}, treatment \eqn{A},
#'   and mediators \eqn{M_1,\ldots, M_k}. If not provided, it is defaulted to a range of 20 values from
#'   -\eqn{\textup{sd}(Y)} to \eqn{\textup{sd}(Y)}, where \eqn{sd} denotes standard deviation and \eqn{Y}
#'   denotes the outcome variable.
#'
#' @param eta_values potential values of the \eqn{\eta} parameter, which denotes the difference in the
#'   prevalence of the unobserved confounder \eqn{U} between treated and untreated units given pretreatment
#'   covariates \eqn{X} and mediators \eqn{M_1,\ldots, M_k}. If not provided, it is defaulted to a range of
#'   20 values from -\eqn{sd(A)} to \eqn{sd(A)}, where \eqn{sd} denotes standard deviation and \eqn{A} denotes
#'   the treatment variable.
#'
#' @return A list containing the following elements \describe{
#'   \item{original}{original estimate of the corresponding path-specific effect.}
#'   \item{adjusted}{a data frame where each row represents a potential combination of \eqn{\gamma} and \eqn{\eta},
#'   the corresponding bias, bias-adjusted estimate, and an indicator for whether the bias-adjusted estimate is
#'   of the opposite sign to the original estimate.}
#'   }
#'
#' @export
#'
#' @references Zhou, Xiang and Teppei Yamamoto. 2020. "\href{https://osf.io/2rx6p}{Tracing Causal Paths from Experimental and Observational Data}".
#'
#' @seealso \code{\link{paths}}, \code{\link{plot.sens}}
#'
#' @example inst/examples/sens.paths-example.R
#'
sens <- function(object,
                 confounded = "M1",
                 estimand = "via M1",
                 estimator = c("pure", "hybrid"),
                 decomp = c("Type I", "Type II"),
                 gamma_values = NULL,
                 eta_values = NULL){

  if(missing(object) || !inherits(object, "paths"))
    stop("'object' must be of class 'paths'.")

  if(missing(estimand))
    stop("'estimand' must be provided.")

  # match arguments
  K <- length(object$varnames$m)
  confounded <- match.arg(confounded, paste0("M", 1:K))
  estimand <- match.arg(estimand, c(paste0("via M", 1:K), "direct"))
  estimator <- match.arg(estimator, c("pure", "hybrid"))
  decomp <- match.arg(decomp, c("Type I", "Type II"))

  # confounded mediator
  j <- as.double(substr(confounded, start = 2, stop = nchar(confounded)))

  # quantity of interest (K+1 corresponds to direct effect)
  k <- if(estimand == "direct") K+1 else as.double(substr(estimand, start = 6, stop = nchar(estimand)))

  # define the ranges of sensitivity parameters
  sd_y <- sd(object$data[[object$varnames$y]])
  sd_a <- sd(object$data[[object$varnames$a]])
  gamma_values <- gamma_values %||% seq(-sd_y, sd_y, length.out = 20)
  eta_values <- eta_values %||% seq(-sd_a, sd_a, length.out = 20)

  df <- expand.grid(`gamma_k` = gamma_values, `eta_k` = eta_values)

  if(k<j){
    df$bias <- 0
  } else if(k==j){
    df$bias <- with(df, - `gamma_k` * `eta_k`)
  } else if(k==K+1){
    # names(df) <- c("gamma_K", "eta_K")
    # df$bias <- with(df, `gamma_K` * `eta_K`)
    df$bias <- with(df, `gamma_k` * `eta_k`)
  } else{
    df <- expand.grid(`gamma_k-1` = gamma_values, `eta_k-1` = eta_values,
                      `gamma_k` = gamma_values, `eta_k` = eta_values)
    df$bias <- with(df, `gamma_k-1` * `eta_k-1` - `gamma_k` * `eta_k`)
  }

  # original effect estimate
  res <- object[[estimator]]
  index <- which(res$estimand == estimand & res$decomp == decomp)
  stopifnot(length(index)==1)
  original <- res[index, , drop = FALSE]

  # adjusted effect estimate
  df$value <- original$estimate - df$bias
  df$lower <- original$lower - df$bias
  df$upper <- original$upper - df$bias
  df$reversed <- (sign(df$value) != sign(original$estimate))

  out <- list(original = original, adjusted = df, k = k)
  class(out) <- "sens"
  out
}
