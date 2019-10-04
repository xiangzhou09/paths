library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(BART)

## TEST DATA ##
expit <- function(x) exp(x)/(1+exp(x))

CCES10_public <- read_dta("CCES10_public.dta")

summary(CCES10_public$strikeo)

peace <- CCES10_public %>%
  dplyr::select(caseid, post, democ, strike, strikef, strikeo, threatc, cost, successc, immoral,
                ally, trade, h1, i1, p1, e1, r1, male, white, age, ed4) %>%
  mutate(ipw = 1)


dat <- peace

x <- c("ally", "trade", "h1", "i1", "p1", "e1", "r1", "male", "white", "age", "ed4")
a <- "democ"
m1 <- c("threatc", "cost", "successc")
m2 <- "immoral"
y <- "strikeo"

formula_y <- as.formula(paste(y, " ~ ", paste(c(x, a, m1, m2), collapse= "+")))
formula_m2 <- as.formula(paste(y, " ~ ", paste(c(x, a, m2), collapse= "+")))
formula_m1 <- as.formula(paste(y, " ~ ", paste(c(x, a), collapse= "+")))

y_mat <- dat[[y]]
x_mat <- data.frame(dat[,c(x, a, m1, m2)]) # pbart cannot handle multiple classes
x_mat_y <- data.frame(dat[,c(x, a, m1, m2)])
x_mat_m2 <- data.frame(dat[,c(x, a, m1)])

model_y <- lm(formula_y, data = dat)
model_m2 <- lm(formula_m2, data = dat)
model_m1 <- lm(formula_m1, data = dat)

model_objects <- list(model_y, model_m2)

model_y_bart <- pbart(x.train = x_mat_y, y.train = y_mat)
model_m2_bart <- pbart(x.train = x_mat_m2, y.train = y_mat)

model_objects <- list(model_y_bart, model_m2_bart)
x.train <- x_mat_y
y.train <- y_mat

formulas <- list(formula_y, formula_m2)
models <- c("lm", "lm")

#######

# First function
paths_fun <- function(model_objects = NULL,
                      formulas = NULL,
                      models = NULL,
                      models_args = NULL,
                      mediators = NULL,
                      #sims = 1000, boot = FALSE,
                      treat = "treat.name",
                      covariates = NULL,
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

  ### Clean up input data

  ## Check to see which input method is used
  if(is.null(model_objects) & is.null(formulas)) {
    stop("Either 'model_objects' or 'formulas' must be supplied")
  } else if(!is.null(model_objects)) {

    ## Option 1: User input a list of model objects

    if(!inherits(model_objects, "list")) {
      stop("'model_objects' must be a list of fitted model objects")
    }
    if(!any(sapply(model_objects, function(x) inherits(x, what = c(lm_names, glm_names, bart_names))))) {
      stop("'model_objects' must be a list containing objects of type 'lm', 'glm' or '*bart'")
    }

    model_y <- model_objects[[1]]

    # Model type indicators
    isLm <- sapply(model_objects, inherits, lm_names)
    isGlm <- sapply(model_objects, inherits, glm_names)
    isBart <- sapply(model_objects, inherits, bart_names)

  } else if(!is.null(formulas)) {

    ## Option 2: User input a list of formula and corresponding methods and arguments

    if(is.null(models)){
      stop("argument 'models' must be supplied along side 'formulas'")
    } else {

      if(inherits(models, "list")) {
        models <- unlist(models)
      }

      if(length(models) != length(formulas)) {
        stop("'formulas' and 'models' must be of equal length")
      }
    }

    isLm <- isGlm <- isBart <- rep(FALSE, length(formulas))

    for(i in 1:length(formulas)) {
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
        x <- model.matrix(formulas[[i]], data)[,-1]
        y <- data[[outcome]]

        args <- c(list(x.train = x,
                       y.train = y),
                  models_args[[i]])

        ## TO-DO: Decide which bart function to use here!

        model_objects[[i]] <- do.call(pbart, args)

        isBart[i] <- TRUE
      }
    }

  }

  # extract vectors of observed outcomes, treatment and
  # a matrix of covariates
  if(isLm[1] | isGlm[1]) {
    y <- model.frame(model_y)[[outcome]]

    mat <- model.matrix(model_y)[, -1, drop = FALSE]
    a <- mat[, treat]==1
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

    mat <- x.train
    a <- mat[, treat]==1
  }

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
  mat_a1 <- mat[a,]
  mat_a0 <- mat[!a,]; mat_a0[,treat] <- 1

  E_y_1_mk_0 <- sapply(length(model_objects):1, function(k) {
    # outcome model conditioning on treatment, mediators M_k and X
    if(isLm[k] | isGlm[k]) {
      y_1_mk_0 <- predict(model_objects[[k]], newdata = data.frame(mat_a0))
    }
    if(isBart[k]) {
      var <- colnames(model_objects[[k]]$varcount)
      y_1_mk_0 <- predict(model_objects[[k]], newdata = mat_a0[,var])[["prob.test.mean"]]
    }

    weighted.mean(y_1_mk_0, w = ipw_a0)
  })

  # construct path-specific effects

  K <- length(model_objects)

  eff_te <- E_a1 - E_a0                                 # Total effect
  eff_a_y <- E_y_1_mk_0[K] - E_a0                       # Direct effect
  eff_a_mk_y <- c(E_a1, E_y_1_mk_0[-K]) - E_y_1_mk_0    # via each of K mediators
  eff_a_M_y <- E_a1 - E_y_1_mk_0[K]                     # via all mediators

  out <- list(eff_te = eff_te,
              eff_a_y = eff_a_y,
              eff_a_mk_y = eff_a_mk_y,
              eff_a_M_y = eff_a_M_y)

  return(out)
}


## Using model objects
paths_fun(list(model_y, model_m2),
          #sims = 1000, boot = FALSE,
          treat = "democ",
          outcome = "strikeo",
          x.train = x_mat,
          y.train = y_mat)

paths_fun(list(model_y_bart, model_m2_bart),
          #sims = 1000, boot = FALSE,
          treat = "democ",
          outcome = "strikeo",
          x.train = x_mat,
          y.train = y_mat)

## Using formula
paths_fun(formulas = list(formula_y, formula_m2),
          models = c("lm", "lm"),
          models_args = list(list(x = TRUE, y = TRUE),
                             list(model = TRUE, x = TRUE, y = TRUE)),
          #sims = 1000, boot = FALSE,
          treat = "democ",
          outcome = "strikeo",
          data = dat,
          x.train = x_mat,
          y.train = y_mat)
