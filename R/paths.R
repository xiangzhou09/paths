#####################################################
# Primary function
#####################################################
paths <- function(formulas = NULL,
                  models = NULL,
                  models_args = NULL,
                  sims = 1000,
                  treat = NULL,
                  outcome = NULL,
                  conditional = TRUE,
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> origin
                  ps = FALSE,
                  ps_formula = update(formulas[[1]], paste(treat, "~ . -", treat)),
                  ps_model = "glm",
                  ps_model_args = list(binomial(link = "logit")),
<<<<<<< HEAD
=======
=======
                  w = NULL,
>>>>>>> origin
>>>>>>> origin
                  conf.level = 0.95,
                  long = TRUE,
                  data = NULL,
                  ...) {

  ### parse out function call ###

  cl <- match.call()

  lm_names <- "lm"
  glm_names <- "glm"
  bart_names <- c("abart", "gbart", "lbart",
                  "pbart", "mbart", "mbart2",
                  "recurbart", "survbart",
                  "wbart")

  ### Extract information from input data ###

  ## Check if formulas are provided
  if(is.null(formulas)) {
    stop("'formulas' must be supplied as a list of model formulas")
  }

  n_models <- length(formulas)
  K <- n_models - 1

  ## Check if model specifications are provided
  if(is.null(models)){
    warning("Argument 'models' is not supplied alongside 'formulas', using 'lm' as default")
    models <- rep("lm", length(formulas))

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

    if(length(models_args) != n_models) {
      stop("Arguments 'formulas' and 'models_args' must be of equal length")
    }
  }


  ## Check model types
  isLm <- models %in% lm_names
  isGlm <- models %in% glm_names
  isBart <- models %in% bart_names

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
        stop("'ps_formula' must contained variables in the main models")
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

      # model_fit requires a list of list of model arguments
      if(!inherits(ps_model_args, "list")) {
        ps_model_args <- list(list(ps_model_args))
      } else if(inherits(ps_model_args, "list") & !inherits(ps_model_args[[1]], "list")) {
        ps_model_args <- list(ps_model_args)
      }

      if(length(ps_model_args) > 1) {
        warning("Only 1 item is allowed for 'ps_model_args' but multiple are supplied. Using the first item only.")
        ps_model_args <- ps_model_args[[1]]
      }
    }

    # Check model type for propensity score model
    ps_isLm <- ps_model %in% lm_names
    ps_isGlm <- ps_model %in% glm_names
    ps_isBart <- ps_model %in% bart_names
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
  model_objects <- model_fit(dat_boot, formulas, models_args, isLm, isGlm, isBart)

  ### Calculate point estimate and bootstrap for uncertainty estimate ###
  boot_out <- boot::boot(data = dat_boot,
                         statistic = paths_fun,
                         R = sims,
                         sim = "ordinary",
                         formulas = formulas,
                         models_args = models_args,
                         treat = treat_var,
                         outcome = outcome_var,
                         conditional = conditional,
                         isLm = isLm,
                         isGlm = isGlm,
                         isBart = isBart,
                         ps = ps,
                         ps_formula = ps_formula,
                         ps_model_args = ps_model_args,
                         ps_isLm = ps_isLm,
                         ps_isGlm = ps_isGlm,
                         ps_isBart = ps_isBart,
                         ...)


  ### Compute outputs and put them together ###

  ## Parameters for confidence interval
  low <- (1 - conf.level)/2
  high <- 1 - low

  ## Extract bootstrap output
  # Original point estimate applied to original data
  eff_te <- boot_out$t0[1]
  eff_a_y_t1 <- boot_out$t0[2]
  eff_a_y_t2 <- boot_out$t0[3]
  eff_a_mk_y_t1 <- boot_out$t0[4:(K+3)]
  eff_a_mk_y_t2 <- boot_out$t0[(K+4):length(boot_out$t0)]

  # Bootstrap replicates
  eff_te_sim <- boot_out$t[, 1, drop = FALSE]
  eff_a_y_t1_sim <- boot_out$t[, 2, drop = FALSE]
  eff_a_y_t2_sim <- boot_out$t[, 3, drop = FALSE]
  eff_a_mk_y_t1_sim <- boot_out$t[, 4:(K+3), drop = FALSE]
  eff_a_mk_y_t2_sim <- boot_out$t[, (K+4):ncol(boot_out$t), drop = FALSE]

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
              w = NULL,
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
              ps = ps)

  if(long == TRUE){
    out[["boot_out"]] = data.frame(eff_te_sim = eff_te_sim,
                                   eff_a_y_t1_sim = eff_a_y_t1_sim, eff_a_y_t2_sim = eff_a_y_t2_sim,
                                   eff_a_mk_y_t1_sim = eff_a_mk_y_t1_sim, eff_a_mk_y_t2_sim = eff_a_mk_y_t2_sim)
  }

  class(out) <- "paths"

  return(out)
}

#### internal function to refit the model given formulas
model_fit <- function(data, formulas, models_args, isLm, isGlm, isBart) {

  n_models <- length(formulas)

  ## Fit a model for each of the input formulas ##
  model_objects <- vector("list", n_models)

  for(i in 1:n_models) {
    # Fit one model object for each formula
    if(isLm[i]) {
      args <- c(list(formula = formulas[[i]],
                     data = data),
                models_args[[i]])

      model_objects[[i]] <- do.call(lm, args)

    } else if(isGlm[i]) {
      args <- c(list(formula = formulas[[i]],
                     data = data),
                models_args[[i]])

      model_objects[[i]] <- do.call(glm, args)

    } else if(isBart[i]) {
      # bart does not automatically convert flexibly named
      # variables in formulas e.g. log(X)
      x.train <- model.matrix(formulas[[i]], data)[,-1]
      y.train <- model.frame(formulas[[i]], data)[,1]

      args <- c(list(x.train = x.train,
                     y.train = y.train),
                models_args[[i]])

      ## TO-DO: Decide which bart function to use here!

      model_objects[[i]] <- do.call(pbart, args)
    }

  }

  return(model_objects)
}

#### internal function to calculate the estimates
<<<<<<< HEAD
paths_fun <- function(data, index, formulas, models_args, treat, outcome, conditional, isLm, isGlm, isBart,
                      ps, ps_formula, ps_model_args, ps_isLm, ps_isGlm, ps_isBart) {
=======
<<<<<<< HEAD
paths_fun <- function(data, index, formulas, models_args, treat, outcome, conditional, isLm, isGlm, isBart,
                      ps, ps_formula, ps_model_args, ps_isLm, ps_isGlm, ps_isBart) {
=======
paths_fun <- function(data, index, formulas, models_args, treat, outcome, conditional, isLm, isGlm, isBart, w = NULL) {
>>>>>>> origin
>>>>>>> origin

  n_models <- length(formulas)
  K <- n_models - 1

  # extract vectors of outcomes, of all variables, and of treatment
  x <- data[index,]

  y <- x[[outcome]]
  a <- x[[treat]]==1

  # Adding weights, if needed
  if(ps) {

    # fit propensity score model and extract weights
    ps_mod <- model_fit(x, ps_formula, ps_model_args, ps_isLm, ps_isGlm, ps_isBart)[[1]]

    ps_score <- fitted(ps_mod)

    # for some models, need to coerce predicted probability to (0,1)
    ps_score[ps_score < 0] <- 0
    ps_score[ps_score > 1] <- 1

    ipw <- a*mean(a)/ps_score + (1-a)*mean(1-a)/(1-ps_score)

  } else {
    ipw <- rep(1, length(y))
  }

  ipw_a1 <- ipw[a]
  ipw_a0 <- ipw[!a]

  ## Estimate components of causal paths
  model_objects <- model_fit(x, formulas, models_args, isLm, isGlm, isBart)

  # Total effect
  if(conditional) {
    # for observational studies, TE must be calculated
    # conditional on X
    x_te <- x

    if(isLm[n_models] | isGlm[n_models]){
      x_te[,treat] <- 1
      E_a1 <- mean(predict(model_objects[[n_models]], newdata = data.frame(x_te)))

      x_te[,treat] <- 0
      E_a0 <- mean(predict(model_objects[[n_models]], newdata = data.frame(x_te)))
    } else if(isBart[n_models]) {
      x_te[,treat] <- 1
      mat_x_te <- model.matrix(formulas[[n_models]], x_te)[,-1]
      E_a1 <- mean(predict(model_objects[[n_models]], newdata = mat_x_te)[["prob.test.mean"]])

      x_te[,treat] <- 0
      mat_x_te <- model.matrix(formulas[[n_models]], x_te)[,-1]
      E_a0 <- mean(predict(model_objects[[n_models]], newdata = mat_x_te)[["prob.test.mean"]])
    }
  } else {
    # for experiments, TE can be calculated unconditionally
    E_a1 <- mean(y[a])
    E_a0 <- mean(y[!a])

  }

  # Decomposition Type 1
  x_a0 <- x[!a,]
  x_a0[,treat] <- 1

  E_y_1_mk_0 <- sapply(K:1, function(k) {
    # predicting outcome conditioning on treatment, mediators M_k and X
    if(isLm[k] | isGlm[k]) {
      y_1_mk_0 <- predict(model_objects[[k]], newdata = data.frame(x_a0))
    } else if(isBart[k]) {
      mat_x_a0 <- model.matrix(formulas[[k]], x_a0)[,-1]
      y_1_mk_0 <- predict(model_objects[[k]], newdata = mat_x_a0)[["prob.test.mean"]]
    }

    weighted.mean(y_1_mk_0, w = ipw_a0)
  })

  # Decomposition Type 2
  x_a1 <- x[a,]
  x_a1[,treat] <- 0

  E_y_0_mk_1 <- sapply(K:1, function(k) {
    # predicting outcome conditioning on treatment, mediators M_k and X
    if(isLm[k] | isGlm[k]) {
      y_0_mk_1 <- predict(model_objects[[k]], newdata = data.frame(x_a1))
    }
    if(isBart[k]) {
      mat_x_a1 <- model.matrix(formulas[[k]], x_a1)[,-1]
      y_0_mk_1 <- predict(model_objects[[k]], newdata = mat_x_a1)[["prob.test.mean"]]
    }

    weighted.mean(y_0_mk_1, w = ipw_a1)
  })

  # construct path-specific effects

  eff_te <- E_a1 - E_a0                                     # Total effect
  eff_a_y_t1 <- E_y_1_mk_0[K] - E_a0                        # Direct effect
  eff_a_y_t2 <- E_a1 - E_y_0_mk_1[K]

  if(K > 1) {
    eff_a_mk_y_t1 <- c(E_a1, E_y_1_mk_0[-K]) - E_y_1_mk_0   # K indirect effects via each of K mediators
    eff_a_mk_y_t2 <- E_y_0_mk_1  - c(E_a0, E_y_0_mk_1[-K])
  } else {
    eff_a_mk_y_t1 <- E_a1 - E_y_1_mk_0[1]                      # If one mediator, only one indirect effect
    eff_a_mk_y_t2 <- E_y_0_mk_1[1] - E_a0
  }

  out <- c(eff_te = eff_te,
           eff_a_y_t1 = eff_a_y_t1,
           eff_a_y_t2 = eff_a_y_t2,
           eff_a_mk_y_t1 = eff_a_mk_y_t1,
           eff_a_mk_y_t2 = eff_a_mk_y_t2)

  return(out)

}

#### internal function to calculate p-values
pval <- function(x, xhat){
  ## Compute p-values
  if (xhat == 0) out <- 1
  else {
    out <- 2 * min(sum(x > 0), sum(x < 0)) / length(x)
  }
  return(min(out, 1))
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

  invisible(x)
}

#####################################################
# Summary method for paths objects
#####################################################
#' @rdname summary.paths
#' @export
summary.paths <- function(x, ...){

  call <- x$call
  treat <- x$treat
  outcome <- x$outcome
  conditional <- x$conditional
  mediators <- x$mediators
  covariates <- x$covariates
  formulas <- x$formulas
  nobs <- nrow(x$data)
  sims <- x$sims
  conf.level <- x$conf.level

  clp <- 100*x$conf.level

  estimates <- cbind(unlist(x$est),
                     unlist(sapply(x$CIs,  function(c) c[1, ])),
                     unlist(sapply(x$CIs,  function(c) c[2, ])),
                     unlist(x$p))

  i_t1 <- c(1, grep("t1", rownames(estimates)))
  i_t2 <- c(1, grep("t2", rownames(estimates)))

  estimates_t1 <- estimates[i_t1,]
  estimates_t2 <- estimates[i_t2,]

  rownames(estimates_t1) <- rownames(estimates_t2) <- c("Total Effect", "Direct Effect",
                           sapply(1:length(x$mediators), function(k) paste("T -> Mediator", k, "->> Y")))
  colnames(estimates_t1) <- colnames(estimates_t2) <- c("Estimate", paste(clp, "% CI Lower", sep=""),
                           paste(clp, "% CI Upper", sep=""), "p-value")

  out <- list(call = call,
              treat = treat,
              outcome = outcome,
              conditional = conditional,
              mediators = mediators,
              covariates = covariates,
              formulas = formulas,
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

  cat("\n\n")
  cat("Sample size:", x$nobs,"\n\n")
  cat("Number of bootstrap simulations:", x$sims,"\n\n")

  invisible(x)
}

#####################################################
# Plot method for paths objects
#####################################################
#' @rdname plot.paths
#' @export
#'
plot.paths <- function(x, mediator_names = NULL, type = c(1,2), ...) {

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


