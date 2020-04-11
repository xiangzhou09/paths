#####################################################
# Primary function
#####################################################
#'
#' Causal Paths Analysis
#'
#' \code{paths} estimates path-specific causal effects in the presence of \eqn{K(\geq 1)} causally
#' ordered mediators. It implements the pure imputation estimator and the imputation-based weighting
#' estimator (when a propensity score model is provided) as detailed in Zhou and Yamamoto (2020).
#' The user supplies the names of the treatment and outcome variables, \eqn{K+1} fitted outcome models given
#' pretreatment confounders and varying sets of mediators, and a data frame containing all the variables. The
#' function returns \eqn{K+1} path-specific causal effects that together constitute the total treatment effect.
#' When \eqn{K=1}, the path-specific causal effects are identical to the natural direct and indirect effects in
#' standard causal mediation analysis (Imai et al. 2010; VanderWeeele 2015).
#'
#' @param a a character string indicating the name of the treatment variable. Only binary treatments
#'   are currently supported.
#'
#' @param y a character string indicating the name of the outcome variable.
#'
#' @param m a list of \eqn{K} character vectors indicating the names of the mediators \eqn{M_1,\ldots, M_K}.
#'
#' @param models a list of \eqn{K+1} fitted model objects describing how the outcome depends on treatment,
#'   pretreatment confounders, and varying sets of mediators, where \eqn{K} is the number of mediators.
#'   \itemize{
#'   \item the first element is a baseline model of the outcome conditional on treatment and potential
#'    pretreatment confounders.
#'   \item the \eqn{k}th element is an outcome model conditional on treatment, pretreatment confounders,
#'   and mediators \eqn{M_1,\ldots, M_{k-1}}.
#'   \item the last element is an outcome model conditional on treatment, pretreatment confounders,
#'   and all of the mediators, i.e., \eqn{M_1,\ldots, M_K}.
#'   }
#'
#'   The fitted model objects can be of type \code{\link{lm}}, \code{\link{glm}}, \code{\link[BART]{wbart}},
#'   or \code{\link[BART]{pbart}}.
#'
#' @param ps_model an optional propensity score model of the treatment. It can be of type \code{\link{glm}}
#'   or \code{\link{BART}{pbart}}. When it is provided, the imputation-based weighting estimator is also used
#'   to compute path-specific causal effects.
#'
#' @param nboot number of bootstrap iterations for estimating confidence intervals. Default is 500.
#'
#' @param conf_level the confidence level of the returned two-sided confidence
#'   intervals. Default is \code{0.95}.
#'
#' @param data a data frame containing all variables.
#'
#' @param ... additional arguments to be passed to \code{boot::boot}, e.g.
#'   \code{parallel} and \code{ncpus}.
#'
#' @return An object of class \code{paths}, which is a list containing the
#'   following elements \describe{
#'   \item{pure}{estimates of direct and path-specific effects via \eqn{M_1, \ldots, M_K}
#'     based on the pure imputation estimator.}
#'   \item{hybrid}{estimates of direct and path-specific effects via \eqn{M_1, \ldots, M_K}
#'     based on the imputation-based weighting estimator.}
#'   \item{varnames}{a list of character strings indicating the names of the pretreatment confounders (\eqn{X}),
#'   treatment(\eqn{A}), mediators (\eqn{M_1, \ldots, M_K}), and outcome (\eqn{Y}).}
#'   \item{call}{the original call to the \code{paths} function.}
#'   }
#'
#' @importFrom BART pbart
#' @importFrom BART wbart
#' @import stats
#' @export
#'
#' @references Zhou, Xiang and Teppei Yamamoto. 2020. "\href{https://osf.io/2rx6p}{Tracing Causal Paths from Experimental and Observational Data}".
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
#' ####################################################
#' # Causal Paths Analysis using BART
#' ####################################################
#'
#' # constructing design matrices for fitting pbart models
#' Y <- tatar[[y]]
#' M0 <- as.matrix(tatar[, c(x, a), drop = FALSE])
#' M1 <- as.matrix(tatar[, c(x, a, m1), drop = FALSE])
#' M2 <- as.matrix(tatar[, c(x, a, m1, m2), drop = FALSE])
#' M3 <- as.matrix(tatar[, c(x, a, m1, m2, m3), drop = FALSE])
#'
#' # outcome models
#' pbart_m0 <- BART::pbart(x.train = M0, y.train = Y)
#' pbart_m1 <- BART::pbart(x.train = M1, y.train = Y)
#' pbart_m2 <- BART::pbart(x.train = M2, y.train = Y)
#' pbart_m3 <- BART::pbart(x.train = M3, y.train = Y)
#' pbart_ymodels <- list(pbart_m0, pbart_m1, pbart_m2, pbart_m3)
#'
#' # causal paths analysis using pbart
#' paths_pbart <- paths(a = "violence", y = "annex", m = mediators,
#'   pbart_ymodels, ps_model = glm_ps, data = tatar,
#'   parallel = "multicore", ncpus = 4, nboot = 4)
paths <- function(a, y, m, models, ps_model = NULL, nboot = 500, conf_level = 0.95, data, ...){

  # Get function call
  cl <- match.call()

  # Check argument missingness and type
  if(missing(a) || !is.character(a) || length(a) > 1)
    stop("'a' must be a character string of length one.")
  if(missing(y) || !is.character(y) || length(y) > 1)
    stop("'y' must be a character string of length one.")
  if(missing(m) || !is.list(m) || any(vapply(m, typeof, character(1)) != "character"))
    stop("'m' must be a list of character vectors.")
  if(missing(models) || !inherits(models, "list") || length(models) < 2)
    stop("'models' must be a list with at least two elements.")
  if(missing(data) || !is.data.frame(data) || ncol(data) < 3)
    stop("'data' must be a data frame with at least three columns.")

  # Check if treatment is binary
  treated <- data[[a]]
  if(any(treated %notin% c(0, 1)))
    stop("Treatment must a binary variable taking 0 or 1 with no missing values.")
  treated <- as.logical(treated)

  # Proportion of units treated
  prop_treated <- mean(treated)
  if(prop_treated==0 || prop_treated==1)
    stop("There must be both treated and untreated units.")

  # Number of mediators
  K <- length(models) - 1

  # Extract classes and families
  classes <- vapply(models, function(m) class(m)[[1]], character(1))
  if(any(classes %notin% c("lm", "glm", "wbart", "pbart")))
    stop("'models' must belong to class 'lm', 'glm', 'wbart', or 'pbart'")
  families <- lapply(models, function(m) m[["family"]])

  # Extract formulas
  formulas <- Map(get_formula, models, classes)
  # mfs <- lapply(formulas, mframe, data = data)

  # Extract covariate names
  vnames <- lapply(formulas, all.vars)
  x <- setdiff(vnames[[K+1]], c(a, y, unlist(m)))

  # Check if all variables are in 'data'
  if(any(vnames[[K+1]] %notin% names(data)))
    stop("all variables must be in 'data'.")

  # Store all variable names in 'varnames'
  varnames <- list(x, a, m, y)
  names(varnames) <- c("x", "a", "m", "y")
  names(varnames$m) <-  paste0("m", 1:K)

  # Extract propensity score model class, family, and formula
  if(!is.null(ps_model)){

    # check ps model
    ps_class <- class(ps_model)[[1]]
    if(ps_class %notin% c("glm", "pbart"))
      stop("'ps_model' must belong to class 'glm' or 'pbart'.")

    ps_family <- ps_model[["family"]]
    ps_formula <- get_formula(ps_model, ps_class)
  } else ps_formula <- ps_class <- ps_family <- NULL

  # Bootstrap to produce point estimates and confidence intervals
  boot_out <- boot::boot(data = data,
                         statistic = paths_fit,
                         R = nboot,
                         sim = "ordinary",
                         varnames = varnames,
                         formulas = formulas,
                         classes = classes,
                         families = families,
                         ps_formula = ps_formula,
                         ps_class = ps_class,
                         ps_family = ps_family,
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
  boot_p <- sapply(1:ncol(boot_out$t), function(i) pval(boot_out$t[,i], boot_out$t0[i]))

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
              families = families,
              ps_formula = ps_formula,
              ps_class = ps_class,
              ps_family = ps_family,
              nboot = nboot,
              conf_level = conf_level,
              data = data,
              call = cl)

  class(out) <- "paths"
  out
}
