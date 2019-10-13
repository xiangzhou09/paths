## TEST DATA ##

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BART)

expit <- function(x) exp(x)/(1+exp(x))

CCES10_public <- read_dta("CCES10_public.dta")

summary(CCES10_public$strikeo)

peace <- CCES10_public %>%
  dplyr::select(caseid, post, democ, strike, strikef, strikeo, threatc, cost, successc, immoral,
                ally, trade, h1, i1, p1, e1, r1, male, white, age, ed4) %>%
  mutate(ipw = 1)


dat <- peace

x <- c("ally", "trade", "h1", "i1", "p1", "e1", "r1", "male", "white", "log(age)", "factor(ed4)")
a <- "democ"
m1 <- c("threatc", "cost", "successc")
m2 <- "immoral"
y <- "strikeo"

formula_y <- as.formula(paste(y, " ~ ", paste(c(x, a, m1, m2), collapse= "+")))
formula_m2 <- as.formula(paste(y, " ~ ", paste(c(x, a, m2), collapse= "+")))
formula_m1 <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))

y_mat <- dat[[y]]
x_mat <- x_mat_y <- model.matrix(formula_y, dat)[,-1] # pbart cannot handle multiple classes
x_mat_m2 <- model.matrix(formula_m2, dat)[,-1]
x_mat_m1 <- model.matrix(formula_m1, dat)[,-1]

model_y <- lm(formula_y, data = dat)
model_m2 <- lm(formula_m2, data = dat)
model_m1 <- lm(formula_m1, data = dat)

model_objects <- list(model_y, model_m2, model_m1)

model_y_bart <- pbart(x.train = x_mat_y, y.train = y_mat)
model_m2_bart <- pbart(x.train = x_mat_m2, y.train = y_mat)
model_m1_bart <- pbart(x.train = x_mat_m1, y.train = y_mat)

model_objects_bart <- list(model_y_bart, model_m2_bart, model_m1_bart)
x.train <- x_mat_y
y.train <- y_mat

model_objects_mix <- list(model_y, model_m2_bart, model_m1)

formulas <- list(formula_y, formula_m2, formula_m1)
models <- c("lm", "pbart", "glm")
models_args <- list(NULL, NULL, list(family = binomial(link = "logit")))

treat <- "democ"

#######

# First function
paths <- function(formulas = NULL,
                  models = NULL,
                  models_args = NULL,
                  sims = 1000,
                  treat = NULL,
                  outcome = NULL,
                  w = NULL,
                  conf.level = 0.95,
                  control.value = 0,
                  treat.value = 1,
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

  ## Check to see which input method is used
  if(is.null(formulas)) {
    stop("'formulas' must be supplied as a list of model formulas")

  } else {

    ### Option 1: User input a list of formula and corresponding methods and arguments ###
    n_models <- length(formulas)
    K <- n_models - 1

    if(is.null(models)){
      warning("Argument 'models' is not supplied along side 'formulas', using 'lm' as default")
      models <- rep("lm", length(formulas))

    } else {

      if(inherits(models, "list")) {
        models <- unlist(models)
      }

      if(length(models) != n_models) {
        stop("'formulas' and 'models' must be of equal length")
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
    for(i in 1:K) {
      mediators_var[[i]] <- dplyr::setdiff(all.vars(formulas[[i]]),
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

    ## Fit a model for each of the input formulas ##
    model_objects <- vector("list", n_models)
    isLm <- isGlm <- isBart <- vector("logical", n_models)

    # strip data to complete cases for the necessary variables only
    data <- na.omit(data[, c(outcome_var, treat_var, unlist(mediators_var), covariates_var)])

    for(i in 1:n_models) {
      # Fit one model object for each formula
      if(models[i] %in% lm_names) {
        args <- c(list(formula = formulas[[i]],
                       data = data),
                  models_args[[i]])

        model_objects[[i]] <- do.call(lm, args)
        isLm[i] <- TRUE

      } else if(models[i] %in% glm_names) {
        args <- c(list(formula = formulas[[i]],
                       data = data),
                  models_args[[i]])

        model_objects[[i]] <- do.call(glm, args)
        isGlm[i] <- TRUE

      } else if(models[i] %in% bart_names) {
        # bart does not automatically convert flexibly named
        # variables in formulas e.g. log(X)
        x.train <- model.matrix(formulas[[i]], data)[,-1]
        y.train <- model.frame(formulas[[i]], data)[,1]

        args <- c(list(x.train = x.train,
                       y.train = y.train),
                  models_args[[i]])

        ## TO-DO: Decide which bart function to use here!

        model_objects[[i]] <- do.call(pbart, args)

        isBart[i] <- TRUE
      }

    }
    # output a dataset to use with the bootstrap
    dat_boot <- data

  }

  ### Calculate point estimate and bootstrap for uncertainty estimate ###
  boot_out <- boot::boot(data = dat_boot,
                         statistic = paths_fun,
                         R = sims,
                         sim = "ordinary",
                         formulas = formulas,
                         model_objects = model_objects,
                         treat = treat_var,
                         outcome = outcome_var,
                         K = K,
                         isLm = isLm,
                         isGlm = isGlm,
                         isBart = isBart,
                         w = w,
                         ...)


  ### Compute outputs and put them together ###

  ## Parameters for confidence interval
  low <- (1 - conf.level)/2
  high <- 1 - low

  ## Extract bootstrap output

  # Original point estimate applied to original data
  eff_te <- boot_out$t0[1]
  eff_a_y <- boot_out$t0[2]
  eff_a_mk_y <- boot_out$t0[-c(1,2)]

  # Bootstrap replicates
  eff_te_sim <- boot_out$t[, 1, drop = FALSE]
  eff_a_y_sim <- boot_out$t[, 2, drop = FALSE]
  eff_a_mk_y_sim <- boot_out$t[, -c(1,2), drop = FALSE]

  # Bootstrap CIs
  eff_CIs <- apply(boot_out$t, 2, function(b) quantile(b, c(low, high), na.rm = TRUE))
  eff_te_CIs <- eff_CIs[, 1, drop = FALSE]
  eff_a_y_CIs <- eff_CIs[, 2, drop = FALSE]
  eff_a_mk_y_CIs <- eff_CIs[, -c(1,2), drop = FALSE]

  # p-values
  eff_te_p <- pval(eff_te_sim, eff_te)
  eff_a_y_p <- pval(eff_a_y_sim, eff_te)
  eff_a_mk_y_p <- mapply(pval, data.frame(eff_a_mk_y_sim), eff_a_mk_y)


  # Unname objects for backward-compatibility
  list2env(
    lapply(list(eff_te = eff_te, eff_a_y = eff_a_y, eff_a_mk_y = eff_a_mk_y,
                eff_a_y_sim = eff_a_y_sim, eff_a_mk_y_sim = eff_a_mk_y_sim,
                eff_te_CIs = eff_te_CIs, eff_a_y_CIs = eff_a_y_CIs, eff_a_mk_y_CIs = eff_a_mk_y_CIs,
                eff_te_p = eff_te_p, eff_a_y_p = eff_a_y_p, eff_a_mk_y_p = eff_a_mk_y_p),
           unname),
    envir = environment()
  )

  out <- list(est = list(eff_te = eff_te, eff_a_y = eff_a_y, eff_a_mk_y = eff_a_mk_y),
              CIs = list(eff_te_CIs = eff_te_CIs, eff_a_y_CIs = eff_a_y_CIs, eff_a_mk_y_CIs = eff_a_mk_y_CIs),
              p = list(eff_te_p = eff_te_p, eff_a_y_p = eff_a_y_p, eff_a_mk_y_p = eff_a_mk_y_p),
              w = NULL,
              call = cl,
              conf.level = conf.level,
              sims = sims,
              outcome = outcome_var, treat = treat_var, mediators = mediators_var, covariates = covariates_var,
              formulas = formulas,
              models = models,
              models_args = models_args,
              model_objects = model_objects,
              data = data)

  if(long == TRUE){
    out[["boot_out"]] = boot_out$t
  }

  class(out) <- "paths"

  return(out)
}

# internal function to calculate the estimates
paths_fun <- function(data, index, formulas, model_objects, treat, outcome, K, isLm, isGlm, isBart, w = NULL) {

  # extract vectors of outcomes, of all variables, and of treatment
  y <- data[[outcome]][index]
  x <- data[index,]
  a <- x[[treat]][index]==1

  #### TODO: Check if the two input methods are consistent ####

  # Adding weights, if needed
  if(!is.null(w)) {
    ipw <- w
  } else {
    ipw <- rep(1, length(y))
  }

  ipw_a1 <- ipw[a]
  ipw_a0 <- ipw[!a]

  # averages of observed outcomes
  E_a1 <- mean(y[a])
  E_a0 <- mean(y[!a])

  # estimate components of causal paths
  x_a1 <- x[a,]
  x_a0 <- x[!a,]; x_a0[,treat] <- 1

  E_y_1_mk_0 <- sapply(K:1, function(k) {
    # predicting outcome conditioning on treatment, mediators M_k and X
    if(isLm[k] | isGlm[k]) {
      y_1_mk_0 <- predict(model_objects[[k]], newdata = data.frame(x_a0))
    }
    if(isBart[k]) {
      mat_x_a0 <- model.matrix(formulas[[k]], x_a0)[,-1]
      y_1_mk_0 <- predict(model_objects[[k]], newdata = mat_x_a0)[["prob.test.mean"]]
    }

    weighted.mean(y_1_mk_0, w = ipw_a0)
  })

  # construct path-specific effects

  eff_te <- E_a1 - E_a0                                 # Total effect
  eff_a_y <- E_y_1_mk_0[K] - E_a0                       # Direct effect
  if(K > 1) {
    eff_a_mk_y <- c(E_a1, E_y_1_mk_0[-K]) - E_y_1_mk_0  # K indirect effects via each of K mediators
  } else {
    eff_a_mk_y <- E_a1 - E_y_1_mk_0[1]                  # If one mediator, only one indirect effect
  }

  out <- c(eff_te = eff_te,
              eff_a_y = eff_a_y,
              eff_a_mk_y = eff_a_mk_y)

  return(out)

}

## Function to compute p-values
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
  names(est) <- c("Total Effect", "Direct Effect",
                  sapply(1:length(x$mediators), function(k) paste("T -> Mediator", k, "->> Y")))
  cat("Causal Paths Estimates: \n")
  print(est)

  invisible(x)
}

#####################################################
# Summary method for paths objects
#####################################################
summary.paths <- function(x, ...){

  call <- x$call
  treat <- x$treat
  outcome <- x$outcome
  mediators <- x$mediators
  formulas <- x$formulas
  nobs <- nrow(x$data)
  sims <- x$sims

  estimates <- cbind(unlist(x$est),
                unlist(sapply(x$CIs,  function(c) c[1, ])),
                unlist(sapply(x$CIs,  function(c) c[2, ])),
                unlist(x$p))

  rownames(estimates) <- c("Total Effect", "Direct Effect",
                      sapply(1:length(x$mediators), function(k) paste("T -> Mediator", k, "->> Y")))
  colnames(estimates) <- c("Estimate", paste(clp, "% CI Lower", sep=""),
                      paste(clp, "% CI Upper", sep=""), "p-value")

  out <- list(call = call,
              treat = treat,
              outcome = outcome,
              mediators = mediators,
              formulas = formulas,
              nobs = nobs,
              sims = sims,
              estimates = estimates)
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

  estimates <- x$estimates

  # Use the printCoefmat() function to conveniently generate
  # summary table
  # Note the use of test statistic-like format
  # (through tst.ind and dig.test) for Estimate and CIs columns
  printCoefmat(estimates,
               digits = 2,
               P.values = TRUE,
               tst.ind = 1:3,
               dig.tst = 3,
               has.Pvalue = TRUE)
  cat("\n\n")
  cat("Sample size:", x$nobs,"\n\n")
  cat("Number of bootstrap simulations:", x$sims,"\n\n")

  invisible(x)
}


# ## Using model objects
# paths(model_objects = list(model_y, model_m2),
#       sims = 10,
#       #boot = FALSE,
#       treat = "democ",
#       outcome = "strikeo",
#       x.train = x_mat,
#       y.train = y_mat)
#
# paths(model_objects = list(model_y_bart, model_m2_bart),
#       #sims = 1000, boot = FALSE,
#       treat = "democ",
#       outcome = "strikeo",
#       x.train = x_mat,
#       y.train = y_mat)
#
# paths(model_objects = list(model_y_bart, model_m2_bart, model_m1_bart),
#       #sims = 1000, boot = FALSE,
#       treat = "democ",
#       outcome = "strikeo",
#       x.train = x_mat,
#       y.train = y_mat)
#
# paths(model_objects = list(model_y, model_m2_bart, model_m1_bart),
#       #sims = 1000, boot = FALSE,
#       treat = "democ",
#       outcome = "strikeo",
#       x.train = x_mat,
#       y.train = y_mat)

## Using formula
path_out <- paths(formulas = list(formula_y, formula_m2),
      models = c("lm", "lm"),
      models_args = list(list(x = TRUE, y = TRUE),
                         list(model = TRUE, x = TRUE, y = TRUE)),
      sims = 1000,
      treat = "democ",
      outcome = "strikeo",
      data = peace)

path_out <- paths(formulas = list(formula_y, formula_m2, formula_m1),
              models = c("lm", "lm", "lm"),
              models_args = list(list(x = TRUE, y = TRUE),
                                 list(model = TRUE, x = TRUE, y = TRUE),
                                 NULL),
              #sims = 1000, boot = FALSE,
              treat = "democ",
              outcome = "strikeo",
              data = peace)

path_out <- paths(formulas = list(formula_y, formula_m2),
      models = c("pbart", "pbart"),
      models_args = list(list(sparse = TRUE),
                         list(theta = 0)),
      sims = 20,
      treat = "democ",
      outcome = "strikeo",
      data = peace,
      w = NULL)

path_out <- paths(formulas = list(formula_y, formula_m2, formula_m1),
      models = c("pbart", "pbart"),
      models_args = list(list(sparse = TRUE),
                         list(theta = 0),
                         list(omega = 1)),
      sims = 20,
      treat = "democ",
      outcome = "strikeo")

