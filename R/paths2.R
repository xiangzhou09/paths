#####################################################
# Primary function
#####################################################
#'
#' Causal Paths Analysis
#'
#' \code{paths2} estimates path-specific causal effects in the presence of \eqn{K(\geq 1)} causally
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
#' @param models a list of \eqn{K+1} fitted model objects describing how the outcome depends on treatment,
#'   pretreatment confounders, and varying sets of mediators, where \eqn{K} is the number of mediators.
#'   \itemize{
#'   \item the first element is a baseline model of the outcome conditional on treatment and pretreatment
#'    confounders.
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
#' @param nboot number of bootstrap iterations for estimating confidence intervals. Default is 1,000.
#'
#' @param conf_level the confidence level of the returned two-sided confidence
#'   intervals. Default is \code{0.95}.
#'
#' @param data a data frame containing all the variables.
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
#'
#' @examples
#'
#' data(tatar)
#'
#' # outcome models being GLM
#' formula_m0 <- annex ~ (kulak + prosoviet_pre + religiosity_pre + house_pre +
#'   land_pre + orchard_pre + animals_pre + carriage_pre + otherprop_pre) * violence
#' formula_m1 <- update(formula_m0,    ~ . + trust_g1 + victim_g1 + fear_g1)
#' formula_m2 <- update(formula_m1,    ~ . + trust_g2 + victim_g2 + fear_g2)
#' formula_m3 <- update(formula_m2,    ~ . + trust_g3 + victim_g3 + fear_g3)
#'
#' formula_ps <- violence ~ kulak + prosoviet_pre + religiosity_pre + house_pre +
#'   land_pre + orchard_pre + animals_pre + carriage_pre + otherprop_pre
#'
#' glm_m0 <- glm(formula_m0, family = quasibinomial("logit"), data = tatar)
#' glm_m1 <- glm(formula_m1, family = quasibinomial("logit"), data = tatar)
#' glm_m2 <- glm(formula_m2, family = quasibinomial("logit"), data = tatar)
#' glm_m3 <- glm(formula_m3, family = quasibinomial("logit"), data = tatar)
#'
#' glm_ps <- glm(formula_ps, family = quasibinomial("logit"), data = tatar)
#'
#' models_glm <- list(glm_m0, glm_m1, glm_m2, glm_m3)
#'
#' out_glm <- paths2(a = "violence", y = "annex", models_glm, ps_model = glm_ps, data = tatar)
#'
#' # outcome models being pbart
#'
#' x <- c("kulak", "prosoviet_pre", "religiosity_pre", "house_pre", "land_pre", "orchard_pre",
#'   "animals_pre", "carriage_pre", "otherprop_pre")
#' a <- "violence"
#' m1 <- c("trust_g1", "victim_g1", "fear_g1")
#' m2 <- c("trust_g2", "victim_g2", "fear_g2")
#' m3 <- c("trust_g3", "victim_g3", "fear_g3")
#' y <- "annex"
#'
#' Y <- tatar[[y]]
#' M0 <- as.matrix(tatar[, c(x, a), drop = FALSE])
#' M1 <- as.matrix(tatar[, c(x, a, m1), drop = FALSE])
#' M2 <- as.matrix(tatar[, c(x, a, m1, m2), drop = FALSE])
#' M3 <- as.matrix(tatar[, c(x, a, m1, m2, m3), drop = FALSE])
#'
#' pbart_m0 <- BART::pbart(x.train = M0, y.train = Y)
#' pbart_m1 <- BART::pbart(x.train = M1, y.train = Y)
#' pbart_m2 <- BART::pbart(x.train = M2, y.train = Y)
#' pbart_m3 <- BART::pbart(x.train = M3, y.train = Y)
#'
#' models_pbart <- list(pbart_m0, pbart_m1, pbart_m2, pbart_m3)
#'
#' out_pbart <- paths2(a, y, models_pbart, ps_model = glm_ps, data = tatar)
#' @export

paths2 <- function(a, y, models, ps_model = NULL, nboot = 1000, conf_level = 0.95, data){

  # get the function call
  cl <- match.call()

  # check argument missingness and type
  if(missing(a) || !is.character(a) || length(a) > 1L) stop("'a' must be a character string of length one.")
  if(missing(y) || !is.character(y) || length(y) > 1L) stop("'y' must be a character string of length one.")
  if(missing(models) || !inherits(models, "list") || length(models) < 2L)
    stop("'models' must be a list with at least two elements.")
  if(missing(data) || !is.data.frame(data) || ncol(data) < 3L)
    stop("'data' must be a data frame with at least three columns.")

  # check if treatment is binary
  treated <- data[[a]]
  if(any(!(treated %in% c(0, 1)))) stop("treatment must a binary variable taking 0 or 1.")
  treated <- as.logical(treated)

  # proportion of units treated
  prop_treated <- mean(treated)
  if(prop_treated==0 || prop_treated==1) stop("there must be both treated and untreated units.")

  # set environments for utility functions
  environment(impute) <- environment(pure) <- environment(hybrid) <- environment()

  # use K to denote the number of mediators
  K <- length(models) - 1L

  # extract model classes, families, and model frames
  classes <- vapply(models, function(m) class(m)[1L], character(1L))
  if(any(!(classes %in% c("lm", "glm", "wbart", "pbart"))))
    stop("models must belong to class 'lm', 'glm', 'wbart', or 'pbart'")
  families <- lapply(models, function(m) m[["family"]])
  mfs <- lapply(models, mframe, data = data)

  # extract covariate names
  vnames <- lapply(mfs, names)
  x <- setdiff(vnames[[1L]], a)

  # store all variable names in 'varnames'
  varnames <- vector("list", K + 3L)
  names(varnames) <- c("x", "a", paste0("m", 1:K), "y")
  varnames[[1L]] <- x
  varnames[[2L]] <- a
  varnames[[K+3L]] <- y
  for (k in seq(1, K)) varnames[[k+2]] <- setdiff(vnames[[k+1]], vnames[[k]])

  # create design matrices for the covariates
  X <- data[, x, drop = FALSE]
  X0 <- X[!treated, , drop = FALSE]
  X1 <- X[treated, , drop = FALSE]

  # imputation for the baseline model
  A0 <- A1 <- mfs[[1L]]
  A0[, a] <- 0; A1[, a] <- 1
  imp_y0 <- pred(object = models[[1L]], newdata = A0)
  imp_y1 <- pred(object = models[[1L]], newdata = A1)
  imp_Ey0 <- mean(imp_y0, na.rm = TRUE)
  imp_Ey1 <- mean(imp_y1, na.rm = TRUE)

  # imputation for the outcome models
  imps <- Map(impute, models[(K+1):2L], mfs[(K+1):2L])

  # pure imputation estimator
  pure_imps <- Map(pure, imps, classes[(K+1):2L], families[(K+1):2L])
  pure_type1 <- c(imp_Ey0, vapply(pure_imps, function(x) x[1L], numeric(1L)), imp_Ey1)
  pure_type2 <- c(imp_Ey0, vapply(pure_imps[K:1L], function(x) x[2L], numeric(1L)), imp_Ey1)
  pure_both <- cbind(`Type I` = pure_type1, `Type II` = pure_type2)
  pure_out  <- `rownames<-`(rbind(apply(pure_both, 2L, diff), imp_Ey1 - imp_Ey0),
                            c("direct", paste0("via M", K:1), "total"))

  # hybrid estimator
  if(!is.null(ps_model)){
    pscore <- pred(ps_model, newdata = data)
    ipw <- prop_treated * treated / pscore + (1 - prop_treated) * (1-treated) / (1-pscore)
    hybrid_imps <- lapply(imps, hybrid)
    hybrid_type1 <- c(imp_Ey0, vapply(hybrid_imps, function(x) x[1L], numeric(1L)), imp_Ey1)
    hybrid_type2 <- c(imp_Ey0, vapply(hybrid_imps[K:1L], function(x) x[2L], numeric(1L)), imp_Ey1)
    hybrid_both <- cbind(`Type I` = hybrid_type1, `Type II` = hybrid_type2)
    hybrid_out  <- `rownames<-`(rbind(apply(hybrid_both, 2L, diff), imp_Ey1 - imp_Ey0),
                                c("direct", paste0("via M", K:1), "total"))
  } else hybrid_out <- NULL

  out <- list(pure = pure_out, hybrid = hybrid_out, varnames = varnames, call = cl)
  class(out) <- "paths"
  out
}
