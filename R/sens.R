#####################################################
# Sensitivity Analysis
#####################################################
#'
#' Sensitivity Analysis for Unobserved Confounding on Path-Specific Effects
#'
#' \code{sens} implements a set of bias formulas detailed in Zhou and Yamamoto (2020) for assessing
#' the sensitivity of estimated path-specific effects to unobserved confounding of the
#' the mediator-outcome relationships. The user provides a fitted \code{paths} object, the mediator
#' whose relationship with the outcome being confounded, the estimand whose sensitivity to unobserved
#' confounding being investigated, type of estimator, type of decomposition, and possible values of
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
#' @param decomposition type of decomposition, \code{"Type I"} or \code{"Type II"}.
#'
#' @param gamma_values potential values of the \eqn{\gamma} parameter. If not provided, it is defaulted to
#' a range of 20 values from -\eqn{\textup{sd}(Y)} to \eqn{\textup{sd}(Y)}, where \eqn{sd} denotes standard
#' deviation and \eqn{Y} denotes the outcome variable.
#'
#' @param eta_values potential values of the \eqn{\eta} parameter. If not provided, it is defaulted to
#' a range of 20 values from -\eqn{sd(A)} to \eqn{sd(A)}, where \eqn{sd} denotes standard deviation and
#' \eqn{Y} denotes the treatment variable.
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
#' @examples
#'
#' data(tatar)
#'
#' x <- c("kulak", "prosoviet_pre", "religiosity_pre", "house_pre", "land_pre", "orchard_pre",
#'   "animals_pre", "carriage_pre", "otherprop_pre")
#' a <- "violence"
#' y <- "annex"
#'
#' m1 <- c("trust_g1", "victim_g1", "fear_g1")
#' m2 <- c("trust_g2", "victim_g2", "fear_g2")
#' m3 <- c("trust_g3", "victim_g3", "fear_g3")
#' mediators <- list(m1, m2, m3)
#'
#' ####################################################
#' # Causal Paths Analysis using GLM
#' ####################################################
#'
#' # outcome model formulas
#' formula_m0 <- annex ~ kulak + prosoviet_pre + religiosity_pre + house_pre +
#'   land_pre + orchard_pre + animals_pre + carriage_pre + otherprop_pre + violence
#' formula_m1 <- update(formula_m0,    ~ . + trust_g1 + victim_g1 + fear_g1)
#' formula_m2 <- update(formula_m1,    ~ . + trust_g2 + victim_g2 + fear_g2)
#' formula_m3 <- update(formula_m2,    ~ . + trust_g3 + victim_g3 + fear_g3)
#'
#' # propensity score model formula
#' formula_ps <- violence ~ kulak + prosoviet_pre + religiosity_pre + house_pre +
#'   land_pre + orchard_pre + animals_pre + carriage_pre + otherprop_pre
#'
#' # outcome models
#' glm_m0 <- glm(formula_m0, family = binomial("logit"), data = tatar)
#' glm_m1 <- glm(formula_m1, family = binomial("logit"), data = tatar)
#' glm_m2 <- glm(formula_m2, family = binomial("logit"), data = tatar)
#' glm_m3 <- glm(formula_m3, family = binomial("logit"), data = tatar)
#' glm_ymodels <- list(glm_m0, glm_m1, glm_m2, glm_m3)
#'
#' # propensity score model
#' glm_ps <- glm(formula_ps, family = binomial("logit"), data = tatar)
#'
#' # causal paths analysis using glm
#' paths_glm <- paths(a = "violence", y = "annex", m = mediators,
#'   glm_ymodels, ps_model = glm_ps, data = tatar,
#'   parallel = "multicore", ncpus = 4, nboot = 100)
#'
#' # sensitivity analysis
#' sens_glm <- sens(paths_glm, confounded = "M1", estimand = "via M1",
#'   gamma_values = - seq(0, 0.5, 0.002), eta_values = seq(-0.5, 0.5, 0.002))
#'
#' # sensitivity analysis plot
#' require(ggplot2)
#' if(requireNamespace("metR", quietly = TRUE)){
#' ggplot(data = sens_glm$adjusted, aes(eta_k, gamma_k)) +
#'   geom_raster(aes(fill = reversed), show.legend = FALSE) +
#'   scale_fill_manual(values = c("white", "grey70")) +
#'   geom_contour(aes(z = value), colour = "black") +
#'   metR::geom_text_contour(aes(z = value), size = 5) +
#'   ylab(expression(paste("Effect of U on Regime Support (", gamma[1], ")"))) +
#'   xlab(expression(paste("Difference in the Prevalence of U between Treated and
#'     Untreated units (", eta[1], ")"))) +
#'   scale_x_continuous(position = "top") +
#'   theme_minimal(base_size = 14)
#'}
sens <- function(object,
                 confounded = "M1",
                 estimand = "via M1",
                 estimator = c("pure", "hybrid"),
                 decomposition = c("Type I", "Type II"),
                 gamma_values = NULL,
                 eta_values = NULL){

  if(missing(object) || !inherits(object, "paths"))
    stop("'object' must be of class 'paths'.")

  if(missing(estimand))
    stop("'estimand' must be provided.")

  # match arguments
  K <- length(object$varnames) - 3
  confounded <- match.arg(confounded, paste0("M", 1:K))
  estimand <- match.arg(estimand, c(paste0("via M", 1:K), "direct"))
  estimator <- match.arg(estimator, c("pure", "hybrid"))
  decomposition <- match.arg(decomposition, c("Type I", "Type II"))

  # confounded mediator
  j <- as.double(substr(confounded, start = 2, stop = nchar(confounded)))

  # quantity of interest (K+1 corresponds to direct effect)
  k <- ifelse(estimand == "direct", K+1, as.double(substr(estimand, start = 6, stop = nchar(estimand))))

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
    names(df) <- c("gamma_K", "eta_K")
    df$bias <- with(df, `gamma_K` * `eta_K`)
  } else{
    df <- expand.grid(`gamma_k-1` = gamma_values, `eta_k-1` = eta_values,
                      `gamma_k` = gamma_values, `eta_k` = eta_values)
    df$bias <- with(df, `gamma_k-1` * `eta_k-1` - `gamma_k` * `eta_k`)
  }

  # original effect estimate
  res <- object[[estimator]]
  index <- which(res$estimand == estimand & res$decomposition == decomposition)
  stopifnot(length(index)==1)
  original <- res[index, , drop = FALSE]

  # adjusted effect estimate
  df$value <- original$estimate - df$bias
  df$lower <- original$lower - df$bias
  df$upper <- original$upper - df$bias
  df$reversed <- (sign(df$value) != sign(original$estimate))

  out <- list(original = original, adjusted = df)
}
