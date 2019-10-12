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
paths <- function(model_objects = NULL,
                  formulas = NULL,
                  models = NULL,
                  models_args = NULL,
                  #mediators = NULL,
                  #sims = 1000, boot = FALSE,
                  treat = NULL,
                  #covariates = NULL,
                  outcome = NULL,
                  w = NULL,
                  data = NULL,
                  x.train = NULL,
                  y.train = NULL) {

  ### parse out function call ###

  cl <- match.call()

  lm_names <- "lm"
  glm_names <- "glm"
  bart_names <- c("abart", "gbart", "lbart",
                  "pbart", "mbart", "mbart2",
                  "recurbart", "survbart",
                  "wbart")

  ### TO-DO: Clean up input data
  ### Decide on best way to use input data, x.train, y.train etc.
  ### and keep only observations that common to the full model


  ## Check to see which input method is used
  if(is.null(model_objects) & is.null(formulas)) {
    stop("Either 'model_objects' or 'formulas' must be supplied")
  } else if(!is.null(formulas)) {

    ### Option 1: User input a list of formula and corresponding methods and arguments ###

    K <- length(formulas)

    if(is.null(models)){
      warning("Argument 'models' is not supplied along side 'formulas', using 'lm' as default")
      models <- rep("lm", length(formulas))

    } else {

      if(inherits(models, "list")) {
        models <- unlist(models)
      }

      if(length(models) != K) {
        stop("'formulas' and 'models' must be of equal length")
      }
    }

    ## Retrieve variable names from formula

    # Retrieve outcome name from formula
    outcome_var <- sapply(formulas, function(f) all.vars(f)[[1]])
    if(length(unique(outcome_var)) > 1) {
      stop("'formulas' must be a list of formulas with the same response variable")
    } else {
      outcome_var <- outcome_var[1]
    }

    # Retrieve mediator names from formula

    mediators_var <- vector("list", K-1)
    for(i in 1:(K-1)) {
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

    # Fit models
    model_objects <- vector("list", K)
    isLm <- isGlm <- isBart <- vector("logical", K)

    # strip data to complete cases for the necessary variables only
    data <- na.omit(data[, c(outcome_var, treat_var, unlist(mediators_var), covariates_var)])

    for(i in 1:K) {
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

  } else if(!is.null(model_objects)) {

    ## Option 2: User input a list of model objects

    K <- length(model_objects)

    if(!inherits(model_objects, "list")) {
      stop("'model_objects' must be a list of fitted model objects")
    }
    if(!any(sapply(model_objects, function(x) inherits(x, what = c(lm_names, glm_names, bart_names))))) {
      stop("'model_objects' must be a list containing objects of type 'lm', 'glm' or '*bart'")
    }

    # Model type indicators
    isLm <- sapply(model_objects, inherits, lm_names)
    isGlm <- sapply(model_objects, inherits, glm_names)
    isBart <- sapply(model_objects, inherits, bart_names)

    ## Retrieve variable names from formula

    # Retrieve outcome name from formula
    outcome_var <- vector("list", K)
    for(i in 1:K) {
      if(isLm[i] | isGlm[i]) {
        outcome_var[i] <- all.vars(formula(model_objects[[i]]))[1]
      } else if(isBart[i]) {
        # not enough information from bart object to identify outcome name
        outcome_var[i] <- NULL
      }
    }

    if(length(unique(unlist(outcome_var))) > 1) {
      stop("'model_objects' must be a list of models with the same response variable")
    } else {
      outcome_var <- unlist(outcome_var)[1]
    }

    if(is.null(outcome_var)) {
      if(is.null(outcome)) {
        warning("No variable name is provided or can be inferred for response variable.
                Consider setting 'outcome' to a value other than NULL.")
        outcome_var <- "y"
      } else {
        outcome_var <- outcome
      }
    }


    # Retrieve mediator names from formula
    mediators_var <- vector("list", K-1)
    all_vars <- vector("list", K)
    for(i in K:1) {
      if(isLm[i] | isGlm[i]) {
        all_vars[[i]] <- all.vars(formula(model_objects[[i]]))[-1]
      } else if (isBart[i]) {
        all_vars[[i]] <- colnames(model_objects[[i]]$varcount)
        # sanitize column names to get variable names
        all_vars[[i]] <- sapply(all_vars[[i]], function(v) gsub(".+\\(", "", v))
        all_vars[[i]] <- sapply(all_vars[[i]], function(v) gsub("\\)", "", v))
        all_vars[[i]] <- sapply(all_vars[[i]], function(v) gsub("\\^.+", "", v))
      }

      if(i < K){
        mediators_var[[i]] <- dplyr::setdiff(all_vars[[i]],
                                             all_vars[[i+1]])
      }
    }

    treat_var <- vector("character", K)

    # Check if treatment name is provided
    if(is.null(treat)) {
      stop("'treat' must be specified")
    } else {
      if(!treat %in% Reduce(intersect, all_vars)) {
        stop("'treat' must be specified in every formula")
      } else {
        # check for variable in dataset with name that matches treat
        # accounting for cases when treat is input flexibly e.g. log(treat)
        treat_var_match <- sapply(all_vars[[1]], function(t) grepl(t, treat, fixed = TRUE))
        treat_var <- all_vars[[1]][treat_var_match][1]
      }
    }

    # Retrieve covariate names from formula
    covariates_var <- dplyr::setdiff(Reduce(intersect, all_vars), c(treat_var, outcome_var))

    # extract observed outcomes, treatment, mediators and covariates
    model_y <- model_objects[[1]]

    if(isLm[1] | isGlm[1]) {
      dat_boot <- model.frame(model_y)
      names(dat_boot) <- all.vars(formula(model_y))

      dat_boot <- dat_boot[, c(outcome_var, treat_var, unlist(mediators_var), covariates_var)]

    } else if(isBart[1]) {
      # Verify that the original data are present
      if(is.null(x.train) | is.null(y.train)) {
        stop("For BART model_objects, the original x.train and y.train are required")
      }
      # Check if the data are at least similar.
      if(nrow(x.train) != length(y.train)) {
        stop("The length of y.train and the number of rows in x.train must be identical")
      }
      if(nrow(x.train) != ncol(model_y$yhat.train)) {
        stop("The length of y.train and the number of rows in x.train must match the length of yhat.train in BART model")
      }
      if(!all(colnames(model_y$varcount) %in% colnames(x.train))) {
        stop("x.train must contain every column used in BART model")
      }

      y <- y.train
      x <- x.train

      dat_boot <- data.frame(y, x)
      names(dat_boot) <- c(outcome_var, colnames(x))
    }

    # output a dataset to use with the bootstrap

  }

  #### TODO: Check if the two input methods are consistent ####

  paths_fun(dat_boot, model_objects, treat_var, outcome_var, K, isLm, isGlm, isBart, w)

  # boot_out <- boot::boot(data = mat,
  #                        statistics = paths_fun,
  #                        R = sims,
  #                        sim = "ordinary",
  #                        K = K,
  #                        isLm = isLm,
  #                        isGlm = isGlm,
  #                        isBart = isBart,
  #                        w = w,
  #                        ...)
  # boot_out <- boot::boot(data = mat,
  #                        statistic = paths_fun,
  #                        R = sims,
  #                        sim = "ordinary",
  #                        K = K,
  #                        isLm = isLm,
  #                        isGlm = isGlm,
  #                        isBart = isBart)


}

# internal function to calculate the estimates
paths_fun <- function(dat, model_objects, treat, outcome, K, isLm, isGlm, isBart, w = NULL) {

  # extract vectors of outcomes, of all variables, and of treatment
  y <- dat[[outcome]]
  x <- dat[, -c(1, which(names(dat) %in% outcome))]
  a <- x[[treat]]==1

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
      # match variable names from BART model (which is taken literally
      # from the original model matrix)
      # with variable names from x_a0 (which corresponds to actual
      # column names from the original data frame)
      bart_var <- colnames(model_objects[[k]]$varcount)
      bart_var_cleaned <- sapply(bart_var, function(v) gsub(".+\\(", "", v))
      bart_var_cleaned <- sapply(bart_var_cleaned, function(v) gsub("\\)", "", v))
      bart_var_cleaned <- sapply(bart_var_cleaned, function(v) gsub("\\^.+", "", v))

      mat_x_a0 <- model.matrix( ~ ., x_a0)[, -1, drop = FALSE]
      colnames(mat_x_a0) <- sapply(colnames(mat_x_a0), function(v) gsub(".+\\(", "", v))
      colnames(mat_x_a0) <- sapply(colnames(mat_x_a0), function(v) gsub("\\)", "", v))
      colnames(mat_x_a0) <- sapply(colnames(mat_x_a0), function(v) gsub("\\^.+", "", v))
      colnames(mat_x_a0) <- sapply(colnames(mat_x_a0), function(v) gsub("`", "", v))

      mat_x_a0 <- mat_x_a0[,bart_var_cleaned]
      colnames(mat_x_a0) <- bart_var

      y_1_mk_0 <- predict(model_objects[[k]], newdata = mat_x_a0)[["prob.test.mean"]]
    }

    weighted.mean(y_1_mk_0, w = ipw_a0)
  })

  # construct path-specific effects

  eff_te <- E_a1 - E_a0                                 # Total effect
  eff_a_y <- E_y_1_mk_0[K] - E_a0                       # Direct effect
  eff_a_mk_y <- c(E_a1, E_y_1_mk_0[-K]) - E_y_1_mk_0    # via each of K mediators
  #eff_a_M_y <- E_a1 - E_y_1_mk_0[K]                     # via all mediators

  out <- list(eff_te = eff_te,
              eff_a_y = eff_a_y,
              eff_a_mk_y = eff_a_mk_y)

  return(out)

}


## Using model objects
paths(model_objects = list(model_y, model_m2),
      #sims = 1000, boot = FALSE,
      treat = "democ",
      outcome = "strikeo",
      x.train = x_mat,
      y.train = y_mat)

paths(model_objects = list(model_y_bart, model_m2_bart),
      #sims = 1000, boot = FALSE,
      treat = "democ",
      outcome = "strikeo",
      x.train = x_mat,
      y.train = y_mat)

paths(model_objects = list(model_y_bart, model_m2_bart, model_m1_bart),
      #sims = 1000, boot = FALSE,
      treat = "democ",
      outcome = "strikeo",
      x.train = x_mat,
      y.train = y_mat)

paths(model_objects = list(model_y, model_m2_bart, model_m1_bart),
      #sims = 1000, boot = FALSE,
      treat = "democ",
      outcome = "strikeo",
      x.train = x_mat,
      y.train = y_mat)

## Using formula
paths(formulas = list(formula_y, formula_m2),
      models = c("lm", "lm"),
      models_args = list(list(x = TRUE, y = TRUE),
                         list(model = TRUE, x = TRUE, y = TRUE)),
      #sims = 1000, boot = FALSE,
      treat = "democ",
      outcome = "strikeo",
      data = dat,
      x.train = x_mat,
      y.train = y_mat)

paths(formulas = list(formula_y, formula_m2),
      models = c("pbart", "pbart"),
      models_args = list(list(sparse = TRUE),
                         list(theta = 0)),
      #sims = 1000, boot = FALSE,
      treat = "democ",
      outcome = "strikeo",
      data = dat,
      x.train = x_mat,
      y.train = y_mat)

paths(formulas = list(formula_y, formula_m2, formula_m1),
      models_args = list(list(sparse = TRUE),
                         list(theta = 0),
                         list(omega = 1)),
      #sims = 1000, boot = FALSE,
      treat = "democ",
      outcome = "strikeo",
      x.train = x_mat,
      y.train = y_mat)
