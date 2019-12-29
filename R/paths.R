#####################################################
# Primary function
#####################################################
#'
#' Causal Paths Analysis
#'
#' @aliases summary.paths print.summary.paths
#'
#' @param formulas a list of \eqn{K+1} formulas where \eqn{K} is the number
#' of mediators in the causal chain.
#' \itemize{
#'   \item The first formula must describe a fully-specified
#'   model of the outcome conditional on all model variables
#'   \\ i.e. \eqn{Y ~ X + A + M_1 + \ldots + M_K}
#'   \item The second formula must describe a model of the
#'   outcome conditional on all model variables
#'   except the last mediator(s) in the causal chain
#'   \\ i.e. \eqn{Y ~ X + A + M_1 + \ldots + M_{K-1}}
#'   \item Each subsequent formula must describe a model of the
#'   outcome conditional on all model variables
#'   except the two last mediator(s) in the causal chain
#'   \\ i.e. \eqn{Y ~ X + A + M_1 + \ldots + M_{K-2}},
#'   \eqn{Y ~ X + A + M_1 + \ldots + M_{K-3}}, etc.
#'   \item The last formula must describe a model of the
#'   outcome conditional on all model variables
#'   except any of the mediator.
#'   \\ i.e. \eqn{Y ~ X + A},
#' }
#' The outcome variable \eqn{Y} can belong to any type that is supported
#' by \code{lm}, \code{glm}, \code{BART::wbart}, \code{BART::pbart},
#' or \code{BART::lbart}, but the model type must be correctly specified
#' in the \code{models} and \code{models_args} arguments
#'
#' The treatment variable \eqn{A} must be a binary variable.
#'
#' The names of the mediator variables and covariates are automatically
#' extracted from the formulas.
#'
#' @param models a list or a vector of \eqn{K+1} strings specifing
#' the method to be used to fit each of the models. Supported methods
#' are \code{lm}, \code{glm}, \code{wbart}, \code{pbart}, and \code{lbart}
#' (the last three methods require the \code{BART} package.)
#'
#' @param models_args a list of \eqn{K+1}, each containing the arguments
#' to be passed to each of the models' fitting methods. For the \code{glm}
#' method, the arguments can be used to specify model family.
#'
#' @param sims number of Bootstrap draws for estimating uncertainty
#'
#' @param treat a character string indicating the name of the treatment
#' variable used in the models. Only binary treatment is currently supported.
#'
#' @param outcome a character string indicating the name of the treatment
#' variable used in the models. The outcome variable must be
#' compatible with the models specified by \code{models}
#' and \code{models_args}.
#'
#' @param conditional a logical value. If \code{FALSE}, the total treatment
#' effect will be estimated unconditionally as a difference of means
#' between the treatment and control groups. This is advised for
#' data obtained from a randomized experiment. If \code{TRUE}, the
#' total treatment effect will be estimated conditionally on the covariates
#' specified in \code{formulas}. This is advised for data obtained
#' from an observational study.
#'
#' @param ps a logical value. If \code{FALSE}, the path-specific causal
#' effects will be estimated using a pure imputation approach. If \code{TRUE},
#' the imputation-based weighting estimator will be used instead.
#'
#' @param ps_formula a formula with the treatment variable as the response
#' variable. This formula must describe the propensity score model used to
#' estimate propensity scores and calculates imputation weights.
#'
#' @param ps_model a character string specifing
#' the method to be used to fit the propensity score model. Supported methods
#' are \code{lm}, \code{glm}, \code{wbart}, \code{pbart}, and \code{lbart}
#'
#' @param ps_model_args a list of arguments to be passed to the propensity
#' score model's fitting method. For the \code{glm} method, the arguments
#' can be used to specify model family.
#'
#' @param conf.level of the returned two-sided confidence intervals. Default is
#' to return the 2.5 and 97.5 percentiles of the bootstraped effect estimates.
#'
#' @param long a logical value. If \code{TRUE}, the output will contain the
#' entire set of bootstrap draws of the effect estimates.
#'
#' @param data a data frame, list or environment containing the variables
#' in the models. Incomplete cases will be automatically dropped.
#'
#' @param ... additional arguments to be passed to \code{boot::boot},
#' e.g. \code{parallel} and \code{ncpus}
#'
#' @return An object of class "paths"
#' @examples
#'
#' data(tatar)
#'
#' x <- c("kulak", "prosoviet_pre", "religiosity_pre",
#'        "house_pre", "land_pre", "orchard_pre",
#'        "animals_pre", "carriage_pre", "otherprop_pre")
#' a <- "violence"
#' l <- c("trust_g1", "victim_g1", "fear_g1")
#' m <- c("trust_g2", "victim_g2", "fear_g2")
#' n <- c("trust_g3", "victim_g3", "fear_g3")
#' y <- "annex"
#'
#' # outcome models
#'
#' formula_y <- as.formula(paste(y, " ~ ", paste(c(x, a, l, m, n), collapse= "+")))
#' formula_m3 <- as.formula(paste(y, " ~ ", paste(c(x, a, l, m), collapse= "+")))
#' formula_m2 <- as.formula(paste(y, " ~ ", paste(c(x, a, l), collapse= "+")))
#' formula_m1 <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))
#' formula_a <- as.formula(paste(a, " ~ ", paste(c(x), collapse= "+")))
#'
#'
#' ## Model as in Yamamoto & Zhou (2020)
#'
#' path_out_annex <- paths(formulas = list(formula_y, formula_m3, formula_m2, formula_m1),
#'                         models = c("pbart", "pbart", "pbart", "pbart"),
#'                         models_args = list(NULL,
#'                                            NULL,
#'                                            NULL,
#'                                            NULL),
#'                         conditional = TRUE,
#'                         ps = TRUE,
#'                         ps_formula = formula_a,
#'                         ps_model = "glm",
#'                         ps_model_args = list(family = binomial(link = "logit")),
#'                         sims = 10,
#'                         treat = "violence",
#'                         outcome = "annex",
#'                         data = tatar)
#'
#' summary(path_out_annex)
#'
#' @export
paths <- function(formulas = NULL,
                  models = NULL,
                  models_args = NULL,
                  sims = 1000,
                  treat = NULL,
                  outcome = NULL,
                  conditional = TRUE,
                  ps = FALSE,
                  ps_formula = update(formulas[[1]], paste(treat, "~ . -", treat)),
                  ps_model = "glm",
                  ps_model_args = list(binomial(link = "logit")),
                  conf.level = 0.95,
                  long = TRUE,
                  data = NULL,
                  ...) {

  ### parse out function call ###

  cl <- match.call()

  ### Extract information from input data ###

  ## Check if formulas are provided
  if(is.null(formulas)|!inherits(formulas, "list")) {
    stop("'formulas' must be supplied as a list of model formulas")
  }

  n_models <- length(formulas)
  K <- n_models - 1

  if(n_models < 2){
    stop("'formulas' must contain at least 2 formulas")
  }

  ## Check if model specifications are provided
  if(is.null(models)){
    warning("Argument 'models' is not supplied alongside 'formulas', using 'lm' as default")
    models <- rep("lm", length(formulas))
    models_args <- NULL

  } else {

    if(inherits(models, "list")) {
      models <- unlist(models)
    }

    if(length(models) != n_models) {
      stop("'formulas' and 'models' must be of equal length")
    }
  }

  if(is.null(models_args)){
    warning("Argument 'models_args' is not supplied, using default specifications for all models")
    models_args <- rep(NULL, length(formulas))

  } else {

    if(!inherits(models_args, "list") | !all(sapply(models_args, function(arg) inherits(arg, "list")|is.null(arg)))) {
      stop("Argument 'models_args' must be a list containing lists of arguments or NULL")
    } else if (any(sapply(models_args, function(x) any(sapply(x, is.null))))) {
      # prevent pbart from crashing if NULL is submitted as an argument
      stop("No list of arguments within 'models_args' can contain NULL")
    }

    if(length(models_args) != n_models) {
      stop("Arguments 'formulas' and 'models_args' must be of equal length")
    }
  }


  ## If propensity score model is provided, check if model specification is correct
  if(is.null(ps)) {
    warning("Argument 'ps' is not provided, defaulting to FALSE")
    ps <- FALSE
  }

  if(ps) {
    if(is.null(ps_formula)) {
      warning("Argument 'ps_formula' is not supplied even though 'ps' is TRUE,
              using all mediators and covariates as default")
      ps_formula <- list(update(formulas[[1]], paste(treat, "~ . -", treat)))
    } else {

      # model_fit requires a list of formula
      if(!inherits(ps_formula, "list")) {
        ps_formula <- list(ps_formula)
      }

      if(length(dplyr::setdiff(all.vars(ps_formula),
                               all.vars(formulas[[1]]))) > 0) {
        stop("'ps_formula' must contain variables in the main models")
      }
    }

    if(is.null(ps_model)){
      warning("Argument 'ps_model' is not supplied, using 'glm(binomial(link = 'logit')' as default")
      ps_model <- "glm"
      ps_model_args <- list(list(binomial(link = "logit")))
    } else {

      if(inherits(ps_model, "list")) {
        ps_model <- unlist(ps_model)
      }

      if(length(ps_model) > 1) {
        warning("Only 1 item is allowed for 'ps_model' but multiple are supplied. Using the first item only.")
        ps_model <- ps_model[1]
      }
    }

    if(is.null(ps_model_args)){
      warning("Argument 'ps_model_arg' is not supplied, using default specifications")

    } else {

      # model_fit requires a list of NULL and/or lists of model arguments
      if(!inherits(ps_model_args, "list")) {
        ps_model_args <- list(list(ps_model_args))
      } else if(!inherits(ps_model_args[[1]], "list") | !is.null(ps_model_args[[1]])) {
        ps_model_args <- list(ps_model_args)
      }

      if(length(ps_model_args) > 1) {
        warning("Only 1 item is allowed for 'ps_model_args' but multiple are supplied. Using the first item only.")
        ps_model_args <- ps_model_args[[1]]
      }
    }

  }


  ## Retrieve variable names from formula ##

  # Retrieve outcome name from formula
  outcome_var <- sapply(formulas, function(f) all.vars(f)[[1]])
  if(length(unique(outcome_var)) > 1) {
    stop("'formulas' must be a list of formulas with the same response variable")
  } else {
    outcome_var <- outcome_var[1]
  }

  # Retrieve mediator names from formula

  mediators_var <- vector("list", K)
  for(i in K:1) {
    mediators_var[[K+1-i]] <- dplyr::setdiff(all.vars(formulas[[i]]),
                                         all.vars(formulas[[i + 1]]))
  }

  # Check if treatment name is provided
  if(is.null(treat)) {
    stop("'treat' must be specified")
  } else {
    if(!treat %in% Reduce(intersect, lapply(formulas, all.vars))) {
      stop("'treat' must be specified in every formula")
    } else {
      # check for variable in dataset with name that matches treat
      # accounting for cases when treat is input flexibly e.g. log(treat)
      treat_var_match <- sapply(all.vars(formulas[[1]]), function(t) grepl(t, treat, fixed = TRUE))
      treat_var <- all.vars(formulas[[1]])[treat_var_match][1]
    }
  }

  # Retrieve covariate names from formula
  covariates_var <- dplyr::setdiff(Reduce(intersect, lapply(formulas, all.vars)), c(treat_var, outcome_var))

  ## Strip data to complete cases for the necessary variables only to prepare for bootstrap
  dat_boot <- na.omit(data[, c(outcome_var, treat_var, unlist(mediators_var), covariates_var)])

  ## Calculate models using original data
  model_objects <- model_fit(dat_boot, formulas, models, models_args)


  ### Calculate point estimate and bootstrap for uncertainty estimate ###
  boot_out <- boot::boot(data = dat_boot,
                         statistic = paths_fun,
                         R = sims,
                         sim = "ordinary",
                         formulas = formulas,
                         models = models,
                         models_args = models_args,
                         treat = treat_var,
                         outcome = outcome_var,
                         conditional = conditional,
                         ps = ps,
                         ps_model = ps_model,
                         ps_formula = ps_formula,
                         ps_model_args = ps_model_args,
                         ...)

  ### Compute outputs and put them together ###

  ## Parameters for confidence interval
  low <- (1 - conf.level)/2
  high <- 1 - low

  ## Extract bootstrap output

  # Bootstrap replicates
  eff_te_sim <- boot_out$t[, 1, drop = FALSE]
  eff_a_y_t1_sim <- boot_out$t[, 2, drop = FALSE]
  eff_a_y_t2_sim <- boot_out$t[, 3, drop = FALSE]
  eff_a_mk_y_t1_sim <- boot_out$t[, 4:(K+3), drop = FALSE]
  eff_a_mk_y_t2_sim <- boot_out$t[, (K+4):ncol(boot_out$t), drop = FALSE]

  # Point estimate using bootstrap mean
  eff_ests <- apply(boot_out$t, 2, mean, na.rm = TRUE)
  eff_te <- eff_ests[1]
  eff_a_y_t1 <- eff_ests[2]
  eff_a_y_t2 <- eff_ests[3]
  eff_a_mk_y_t1 <- eff_ests[4:(K+3)]
  eff_a_mk_y_t2 <- eff_ests[(K+4):ncol(boot_out$t)]

  # Bootstrap CIs
  eff_CIs <- apply(boot_out$t, 2, function(b) quantile(b, c(low, high), na.rm = TRUE))
  eff_te_CIs <- eff_CIs[, 1, drop = FALSE]
  eff_a_y_t1_CIs <- eff_CIs[, 2, drop = FALSE]
  eff_a_y_t2_CIs <- eff_CIs[, 3, drop = FALSE]
  eff_a_mk_y_t1_CIs <- eff_CIs[, 4:(K+3), drop = FALSE]
  eff_a_mk_y_t2_CIs <- eff_CIs[, (K+4):ncol(boot_out$t), drop = FALSE]

  # p-values
  eff_te_p <- pval(eff_te_sim, eff_te)
  eff_a_y_t1_p <- pval(eff_a_y_t1_sim, eff_a_y_t1)
  eff_a_y_t2_p <- pval(eff_a_y_t2_sim, eff_a_y_t2)
  eff_a_mk_y_t1_p <- mapply(pval, data.frame(eff_a_mk_y_t1_sim), eff_a_mk_y_t1)
  eff_a_mk_y_t2_p <- mapply(pval, data.frame(eff_a_mk_y_t2_sim), eff_a_mk_y_t2)


  # Unname objects for backward-compatibility
  list2env(
    lapply(list(eff_te = eff_te,
                eff_a_y_t1 = eff_a_y_t1, eff_a_y_t2 = eff_a_y_t2,
                eff_a_mk_y_t1 = eff_a_mk_y_t1, eff_a_mk_y_t2 = eff_a_mk_y_t2,
                eff_te_sim = eff_te_sim,
                eff_a_y_t1_sim = eff_a_y_t1_sim, eff_a_y_t2_sim = eff_a_y_t2_sim,
                eff_a_mk_y_t1_sim = eff_a_mk_y_t1_sim, eff_a_mk_y_t2_sim = eff_a_mk_y_t2_sim,
                eff_te_CIs = eff_te_CIs,
                eff_a_y_t1_CIs = eff_a_y_t1_CIs, eff_a_y_t2_CIs = eff_a_y_t2_CIs,
                eff_a_mk_y_t1_CIs = eff_a_mk_y_t1_CIs, eff_a_mk_y_t2_CIs = eff_a_mk_y_t2_CIs,
                eff_te_p = eff_te_p,
                eff_a_y_t1_p = eff_a_y_t1_p, eff_a_y_t2_p = eff_a_y_t2_p,
                eff_a_mk_y_t1_p = eff_a_mk_y_t1_p, eff_a_mk_y_t2_p = eff_a_mk_y_t2_p),
           unname),
    envir = environment()
  )

  out <- list(est = list(eff_te = eff_te,
                         eff_a_y_t1 = eff_a_y_t1, eff_a_mk_y_t1 = eff_a_mk_y_t1,
                         eff_a_y_t2 = eff_a_y_t2, eff_a_mk_y_t2 = eff_a_mk_y_t2),
              CIs = list(eff_te_CIs = eff_te_CIs,
                         eff_a_y_t1_CIs = eff_a_y_t1_CIs, eff_a_mk_y_t1_CIs = eff_a_mk_y_t1_CIs,
                         eff_a_y_t2_CIs = eff_a_y_t2_CIs, eff_a_mk_y_t2_CIs = eff_a_mk_y_t2_CIs),
              p = list(eff_te_p = eff_te_p,
                       eff_a_y_t1_p = eff_a_y_t1_p, eff_a_mk_y_t1_p = eff_a_mk_y_t1_p,
                       eff_a_y_t2_p = eff_a_y_t2_p, eff_a_mk_y_t2_p = eff_a_mk_y_t2_p),
              call = cl,
              conf.level = conf.level,
              sims = sims,
              outcome = outcome_var, treat = treat_var, mediators = mediators_var, covariates = covariates_var,
              conditional = conditional,
              formulas = formulas,
              models = models,
              models_args = models_args,
              model_objects = model_objects,
              data = data,
              ps = ps,
              ps_formula = ps_formula,
              ps_model = ps_model,
              ps_model_args = ps_model_args)

  if(long == TRUE){
    out[["boot_out"]] = data.frame(eff_te_sim = eff_te_sim,
                                   eff_a_y_t1_sim = eff_a_y_t1_sim, eff_a_y_t2_sim = eff_a_y_t2_sim,
                                   eff_a_mk_y_t1_sim = eff_a_mk_y_t1_sim, eff_a_mk_y_t2_sim = eff_a_mk_y_t2_sim)
  }

  class(out) <- "paths"

  return(out)
}

#####################################################
# Print method for paths objects
#####################################################
print.paths <- function(x, ...) {
  cat("\n")

  cat("Causal Paths Analysis \n\n")

  # Print function calll
  cat("Call: ")
  print(x$call)
  cat("\n")

  # Print model variables
  cat("Treatment:", x$treat, "\n")
  cat("Outcome:", x$outcome, "\n\n")

  cat("Outcome model: ")
  print(x$formulas[[1]])
  cat("\n")

  for(i in 1:length(x$mediators)) {
    cat("Mediator ", i, ": ", paste(x$mediators[[i]], collapse = " + "), "\n", sep ="")
  }
  cat("\n")

  # Print effect estimates
  est <- unlist(x$est)

  i_t1 <- c(1, grep("t1", names(est)))
  i_t2 <- c(1, grep("t2", names(est)))
  est_t1 <- est[i_t1]
  est_t2 <- est[i_t2]

  names(est_t1) <- names(est_t2) <- c("Total Effect", "Direct Effect",
                  sapply(1:length(x$mediators), function(k) paste("T -> Mediator", k, "->> Y")))
  cat("Causal Paths Estimates: \n")
  cat("Type 1 Decomposition: \n")
  print(est_t1)
  cat("\n")
  cat("Type 2 Decomposition: \n")
  print(est_t2)

  cat("\n")
  if(x$conditional){
    cat("Total Effect calculated conditional on covariates \n")
  } else {
    cat("Total Effect calculated unconditionally \n")
  }

  cat("\n")
  if(x$ps){
    cat("Paths Estimates calculated using IPW \n")
    cat("Propensity score model: ")
    print(x$ps_formula[[1]])

  } else {
    cat("Paths Estimates calculated using imputation method \n")
  }

  invisible(x)
}

#####################################################
# Summary method for paths objects
#####################################################
#' Summarizing Output from Causal Paths Analysis
#'
#' @aliases print.summary.paths
#' @export
summary.paths <- function(object, ...){

  call <- object$call
  treat <- object$treat
  outcome <- object$outcome
  conditional <- object$conditional
  mediators <- object$mediators
  covariates <- object$covariates
  formulas <- object$formulas
  ps <- object$ps
  ps_formula <- object$ps_formula
  nobs <- nrow(object$data)
  sims <- object$sims
  conf.level <- object$conf.level

  clp <- 100*object$conf.level

  estimates <- cbind(unlist(object$est),
                     unlist(sapply(object$CIs,  function(c) c[1, ])),
                     unlist(sapply(object$CIs,  function(c) c[2, ])),
                     unlist(object$p))

  i_t1 <- c(1, grep("t1", rownames(estimates)))
  i_t2 <- c(1, grep("t2", rownames(estimates)))

  estimates_t1 <- estimates[i_t1,]
  estimates_t2 <- estimates[i_t2,]

  rownames(estimates_t1) <- rownames(estimates_t2) <- c("Total Effect", "Direct Effect",
                           sapply(1:length(object$mediators), function(k) paste("T -> Mediator", k, "->> Y")))
  colnames(estimates_t1) <- colnames(estimates_t2) <- c("Estimate", paste(clp, "% CI Lower", sep=""),
                           paste(clp, "% CI Upper", sep=""), "p-value")

  out <- list(call = call,
              treat = treat,
              outcome = outcome,
              conditional = conditional,
              mediators = mediators,
              covariates = covariates,
              formulas = formulas,
              ps = ps,
              ps_formula = ps_formula,
              nobs = nobs,
              sims = sims,
              conf.level = conf.level,
              estimates = list(estimates_t1 = estimates_t1,
                               estimates_t2 = estimates_t2))
  class(out) <- "summary.paths"

  return(out)
}

#' @rdname summary.paths
#' @export
print.summary.paths <- function(x, ...) {

  clp <- 100*x$conf.level

  cat("\n")

  cat("Causal Paths Analysis \n\n")

  # Print function calll
  cat("Call: ")
  print(x$call)
  cat("\n")

  # Print model variables
  cat("Treatment:", x$treat, "\n")
  cat("Outcome:", x$outcome, "\n\n")

  cat("Outcome model: ")
  print(x$formulas[[1]])
  cat("\n")

  for(i in 1:length(x$mediators)) {
    cat("Mediator ", i, ": ", paste(x$mediators[[i]], collapse = " + "), "\n", sep ="")
  }
  cat("\n")

  # Print output table

  estimates_t1 <- x$estimates$estimates_t1
  estimates_t2 <- x$estimates$estimates_t2

  # Use the printCoefmat() function to conveniently generate
  # summary table
  # Note the use of test statistic-like format
  # (through tst.ind and dig.test) for Estimate and CIs columns
  cat("Type 1 Decomposition: \n")
  printCoefmat(estimates_t1,
               digits = 2,
               P.values = TRUE,
               tst.ind = 1:3,
               dig.tst = 3,
               has.Pvalue = TRUE)

  cat("\n\n")
  cat("Type 2 Decomposition: \n")
  printCoefmat(estimates_t2,
               digits = 2,
               P.values = TRUE,
               tst.ind = 1:3,
               dig.tst = 3,
               has.Pvalue = TRUE)

  cat("\n")
  if(x$conditional){
    cat("Total Effect calculated conditional on covariates \n")
  } else {
    cat("Total Effect calculated unconditionally \n")
  }

  cat("\n")
  if(x$ps){
    cat("Paths Estimates calculated using IPW \n")
    cat("Propensity score model: ")
    print(x$ps_formula[[1]])

  } else {
    cat("Paths Estimates calculated using imputation method \n")
  }

  cat("\n\n")
  cat("Sample size:", x$nobs,"\n\n")
  cat("Number of bootstrap simulations:", x$sims,"\n\n")

  invisible(x)
}

#####################################################
# Plot method for paths objects
#####################################################
#'
#' Plot Method for \code{paths} Objects
#'
#' Plot point estimates and confidence intervals
#' for each individual path-specific effect
#' from a \code{paths} object.
#'
#' @param x object of class \code{paths} as produced by the
#'   \code{paths} function
#'
#' @param mediator_names a vector of strings indicating the
#'   labels for each mediator. Must contain as many elements
#'   as there are mediators in the model. If not supplied,
#'   the default labels will be constructed from the model
#'   formulas supplied to the \code{paths} function.
#'
#' @param type either \code{1}, \code{2}, or \code{c(1,2)},
#'   indicating whether the plot will display estimates obtained
#'   using Type I, Type II, or both Type I and Type II decompsitions.
#'   Default is to show estimates from both types.
#'
#'
#' @rdname plot.paths
#'
#' @export
#'
plot.paths <- function(x, mediator_names = NULL, type = c(1,2)) {

  if(is.null(mediator_names)){
    mediators <- sapply(x$mediators, function(m) paste(m, collapse = " + "))
  } else {
    if(length(mediator_names) != length(x$mediators)) {
      stop("'mediator_names' must have the same length with 'mediators'")
    }
    mediators <- mediator_names
  }

  if(any(!unique(type) %in% c(1,2))) {
    stop("'type' must be either 1, 2, or c(1,2)")
  }

  estimand <- c("Total Effect", "Direct Effect",
                paste("via", mediators))

  estimates <- data.frame(est = unlist(x$est),
                     lower = unlist(sapply(x$CIs,  function(c) c[1, ])),
                     upper = unlist(sapply(x$CIs,  function(c) c[2, ])),
                     p = unlist(x$p))

  i_t1 <- c(1, grep("t1", rownames(estimates)))
  i_t2 <- c(1, grep("t2", rownames(estimates)))

  plot_data_t1 <- data.frame(estimates[i_t1,],
                        estimand = factor(estimand, levels = rev(estimand)),
                        decomposition = "Type 1")

  plot_data_t2 <- data.frame(estimates[i_t2,],
                        estimand = factor(estimand, levels = rev(estimand)),
                        decomposition = "Type 2")

  if(identical(type, 1)) {
    plot_data <- plot_data_t1
    type_legend <- scale_color_manual(guide = FALSE, values = c("black"))

  } else if(identical(type, 2)) {
    plot_data <- plot_data_t2
    type_legend <- scale_color_manual(guide = FALSE, values = c("black"))

  } else if(identical(type, c(1,2))) {
    plot_data <- rbind(plot_data_t1,
                       plot_data_t2[-1,])
    type_legend <- scale_color_manual(labels = c("Type 1 Decomposition", "Type 2 Decomposition"),
                                      values = c("blue", "red"))
  }


  ggplot(plot_data, aes(x = estimand, y = est, colour = decomposition)) +
    geom_pointrange(aes(ymin = lower, ymax = upper), size = 1,
                    position = position_dodge2(width = .5)) +
    geom_vline(xintercept = 0, linetype = 2) +
    xlab("") +
    ylab("Estimates of Total and Path-Specific Effects") +
    coord_flip() +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    type_legend

}
