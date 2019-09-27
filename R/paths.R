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

models <- list(model_y, model_m2)

model_y_bart <- pbart(x.train = x_mat_y, y.train = y_mat)
model_m2_bart <- pbart(x.train = x_mat_m2, y.train = y_mat)

models <- list(model_y_bart, model_m2_bart)
x.train <- x_mat_y
y.train <- y_mat

paths_fun <- function(models = list(),
                  sims = 1000, boot = FALSE,
                  treat = "treat.name",
                  #mediator = list(c("m1_name_1", ...), ...),
                  covariates = NULL,
                  outcome = NULL,
                  w = NULL,
                  x.train = NULL,
                  y.train = NULL) {

  model_y <- models[[1]]

  # Model type indicators
  isLm <- sapply(models, inherits, "lm")
  isGlm <- sapply(models, inherits, "glm")
  isBart <- sapply(models, inherits, c("abart", "gbart", "lbart",
                                       "pbart", "mbart", "mbart2",
                                       "recurbart", "survbart",
                                       "wbart"))

  # extract vectors of observed outcomes, treatment and
  # a matrix of covariates
  if(isLm[1] | isGlm[1]) {
    y <- model.frame(model_y)[[outcome]]

    mat <- model.matrix(model_y)[, -1, drop = FALSE]
    a <- mat[, treat]==1
  } else if(isBart[1]) {
    # Verify that the original data are present
    if(is.null(x.train) | is.null(y.train)) {
      stop("For BART models, the original x.train and y.train are required")
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

  E_y_1_mk_0 <- sapply(length(models):1, function(k) {
    # outcome model conditioning on treatment, mediators M_k and X
    if(isLm[k] | isGlm[k]) {
      y_1_mk_0 <- predict(models[[k]], newdata = data.frame(mat_a0))
    }
    if(isBart[k]) {
      var <- colnames(models[[k]]$varcount)
      y_1_mk_0 <- predict(models[[k]], newdata = mat_a0[,var])[["prob.test.mean"]]
    }

    weighted.mean(y_1_mk_0, w = ipw_a0)
  })

  # construct path-specific effects

  K <- length(models)

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

paths_fun(list(model_y, model_m2),
          sims = 1000, boot = FALSE,
          treat = "democ",
          outcome = "strikeo",
          x.train = x_mat,
          y.train = y_mat)

paths_fun(list(model_y_bart, model_m2_bart),
          sims = 1000, boot = FALSE,
          treat = "democ",
          outcome = "strikeo",
          x.train = x_mat,
          y.train = y_mat)
