#####################################################
# Primary function
#####################################################
#'
#' Causal Paths Analysis
#'
#' \code{paths} estimates path-specific causal effects in the presence of \eqn{K(\geq 1)} causally
#' ordered mediators. It implements the pure imputation estimator and the imputation-based weighting
#' estimator (when a propensity score model is provided) as detailed in Zhou and Yamamoto (2020).
#' The user supplies the names of the treatment, outcome, mediator variables, \eqn{K+1} fitted models
#' characterizing the conditional mean of the outcome given treatment, pretreatment confounders, and
#' varying sets of mediators, and a data frame containing all the variables. The function returns
#' \eqn{K+1} path-specific causal effects that together constitute the total treatment effect.
#' When \eqn{K=1}, the path-specific causal effects are identical to the natural direct and indirect
#' effects in standard causal mediation analysis.
#'
#' @param a a character string indicating the name of the treatment variable. The treatment
#'   should be a binary variable taking either 0 or 1.
#'
#' @param y a character string indicating the name of the outcome variable.
#'
#' @param m a list of \eqn{K} character vectors indicating the names of \eqn{K} causally ordered mediators
#'   \eqn{M_1,\ldots, M_K}.
#'
#' @param models a list of \eqn{K+1} fitted model objects describing how the outcome depends on treatment,
#'   pretreatment confounders, and varying sets of mediators, where \eqn{K} is the number of mediators.
#'   \itemize{
#'   \item the first element is a baseline model of the outcome conditional on treatment and pretreatment
#'   confounders.
#'   \item the \eqn{k}th element is an outcome model conditional on treatment, pretreatment confounders,
#'   and mediators \eqn{M_1,\ldots, M_{k-1}}.
#'   \item the last element is an outcome model conditional on treatment, pretreatment confounders,
#'   and all of the mediators, i.e., \eqn{M_1,\ldots, M_K}.
#'   }
#'
#'   The fitted model objects can be of type \code{\link{lm}}, \code{\link{glm}}, \code{\link[gbm]{gbm}},
#'   \code{\link[BART]{wbart}}, or \code{\link[BART]{pbart}}.
#'
#' @param ps_model an optional propensity score model for treatment. It can be of type \code{\link{glm}},
#'   \code{\link[gbm]{gbm}}, \code{\link[twang]{ps}}, or \code{\link[BART]{pbart}}. When it is provided,
#'   the imputation-based weighting estimator is also used to compute path-specific causal effects.
#'
#' @param nboot number of bootstrap iterations for estimating confidence intervals. Default is 500.
#'
#' @param conf_level the confidence level of the returned two-sided confidence
#'   intervals. Default is \code{0.95}.
#'
#' @param data a data frame containing all variables.
#'
#' @param ... additional arguments to be passed to \code{boot::boot}, e.g.
#'   \code{parallel} and \code{ncpus}. For the \code{print} method, additional arguments to be passed to
#'   \code{print.default}
#'
#' @return An object of class \code{paths}, which is a list containing the
#'   following elements \describe{
#'   \item{pure}{estimates of direct and path-specific effects via \eqn{M_1, \ldots, M_K}
#'     based on the pure imputation estimator.}
#'   \item{hybrid}{estimates of direct and path-specific effects via \eqn{M_1, \ldots, M_K}
#'     based on the imputation-based weighting estimator.}
#'   \item{varnames}{a list of character strings indicating the names of the pretreatment confounders (\eqn{X}),
#'   treatment(\eqn{A}), mediators (\eqn{M_1, \ldots, M_K}), and outcome (\eqn{Y}).}
#'   \item{formulas}{formulas for the outcome models.}
#'   \item{classes}{classes of the outcome models.}
#'   \item{families}{model families of the outcome models.}
#'   \item{args}{a list containing arguments of the outcome models.}
#'   \item{ps_formula}{formula for the propensity score model.}
#'   \item{ps_class}{class of the propensity score model.}
#'   \item{ps_family}{model family of the propensity score model.}
#'   \item{ps_args}{arguments of the propensity score model.}
#'   \item{data}{the original data.}
#'   \item{nboot}{number of bootstrap iterations.}
#'   \item{conf_level}{confidence level for confidence intervals.}
#'   \item{boot_out}{output matrix from the bootstrap iterations.}
#'   \item{call}{the matched call to the \code{paths} function.}
#'   }
#'
#' @importFrom gbm gbm
#' @importFrom twang ps
#' @importFrom BART pbart
#' @importFrom BART wbart
#' @import stats
#' @export
#'
#' @references Zhou, Xiang and Teppei Yamamoto. 2020. "\href{https://osf.io/2rx6p}{Tracing Causal Paths from Experimental and Observational Data}".
#'
#' @example inst/examples/paths-example.R
#'
#' @seealso \code{\link{summary.paths}}, \code{\link{plot.paths}}, \code{\link{sens}}
#'
paths <- function(a, y, m, models, ps_model = NULL, data, nboot = 500, conf_level = 0.95, ...){

  # Get function call
  cl <- match.call()

  # Check argument missingness and type
  if(missing(a) || !is.character(a) || length(a) > 1)
    stop("'a' must be a character string of length one.")
  if(missing(y) || !is.character(y) || length(y) > 1)
    stop("'y' must be a character string of length one.")
  if(missing(m) || !is.list(m) || any(vapply(m, typeof, character(1)) != "character"))
    stop("'m' must be a list of character vectors.")
  if(missing(models) || !is.list(models) || length(models) < 2)
    stop("'models' must be a list with at least two elements.")
  if(missing(data) || !is.data.frame(data) || ncol(data) < 3)
    stop("'data' must be a data frame with at least three columns.")

  # Check if treatment is binary
  treated <- data[[a]]
  if(any(treated %notin% c(0, 1)))
    stop("Treatment must be a binary variable taking 0 or 1 with no missing values.")
  treated <- as.logical(treated)

  # Proportion of units treated
  prop_treated <- mean(treated)
  if(prop_treated==0 || prop_treated==1)
    stop("There must be both treated and untreated units.")

  # Number of mediators
  K <- length(m)

  # Extract classes, families, args, and formulas
  classes <- vapply(models, function(mod) class(mod)[[1]], character(1))
  if(any(classes %notin% c("lm", "glm", "gbm", "wbart", "pbart")))
    stop("'models' must belong to class 'lm', 'glm', 'gbm', 'wbart', or 'pbart'")
  families <- lapply(models, function(mod) mod[["family"]])
  args <- Map(get_args, models, classes)
  formulas <- Map(get_formula, models, classes)

  # Extract names of pretreatment covariates
  vnames <- lapply(formulas, all.vars)
  x <- setdiff(vnames[[K+1]], c(a, y, unlist(m)))

  # Check if all variables are in 'data'
  if(any(vnames[[K+1]] %notin% names(data))) stop("all variables must be in 'data'.")

  # Store all variable names in 'varnames'
  varnames <- list(x, a, m, y)
  names(varnames) <- c("x", "a", "m", "y")
  names(varnames$m) <-  paste0("m", 1:K)

  # Extract propensity score model class, family, and formula
  if(!is.null(ps_model)){

    # Check ps model
    ps_class <- class(ps_model)[[1]]
    if(ps_class %notin% c("glm", "gbm", "ps", "pbart"))
      stop("'ps_model' must belong to class 'glm', 'gbm', 'ps', or 'pbart'.")
    ps_family <- ps_model[["family"]]
    ps_args <- get_args(ps_model, ps_class)
    ps_formula <- get_formula(ps_model, ps_class)
  } else ps_formula <- ps_args <- ps_family <- ps_class <- NULL

  # Bootstrap to produce point estimates and confidence intervals
  boot_out <- boot::boot(data = data,
                         statistic = paths_fit,
                         R = nboot,
                         sim = "ordinary",
                         varnames = varnames,
                         formulas = formulas,
                         classes = classes,
                         families = families,
                         args = args,
                         ps_formula = ps_formula,
                         ps_class = ps_class,
                         ps_family = ps_family,
                         ps_args = ps_args,
                         ...)
  if(sink.number()>0) invisible(replicate(sink.number(), sink()))

  # Parameters for confidence interval
  low <- (1 - conf_level)/2
  high <- 1 - low

  # Bootstrap confidence intervals
  boot_se <- apply(boot_out$t, 2, sd, na.rm = TRUE)
  boot_CI <- t(apply(boot_out$t, 2, quantile, c(low, high), na.rm = TRUE))
  colnames(boot_CI) <- c("lower", "upper")

  # Bootstrap p-values
  boot_p <- apply(boot_out$t, 2, pval)

  # Estimates of path-specific and total effects
  pse <- data.frame('names' = names(boot_out$t0), 'estimate' = boot_out$t0,
                    'se' = boot_se, boot_CI, 'p' = boot_p, row.names = NULL, check.names = FALSE)
  pse <- tidyr::separate(pse, names, into = c("estimator", "decomposition", "estimand"), sep = "_")
  pse_pure <- pse[pse$estimator == "pure", , drop = FALSE]
  pse_hybrid <- pse[pse$estimator == "hybrid", , drop = FALSE]

  # Model output
  out <- list(pure = pse_pure,
              hybrid = pse_hybrid,
              varnames = varnames,
              formulas = formulas,
              classes = classes,
              args = args,
              ps_formula = ps_formula,
              ps_class = ps_class,
              ps_args = ps_args,
              data = data,
              nboot = nboot,
              conf_level = conf_level,
              boot_out = boot_out$t,
              call = cl)

  class(out) <- "paths"
  out
}
