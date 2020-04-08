#####################################################
# Utility functions
#####################################################

utils::globalVariables(c("a", "x", "y", "X", "X0", "X1",
                         "ipw", "treated"))

`%notin%` <- Negate(`%in%`)

# logical or infix function
`%||%` <- function(a, b) if (!is.null(a)) a else b

# get args (except for pbart and wbart)
get_args <- function(object, class){
  if(class %in% c("lm", "glm", "gbm")){
    args <- object$call
    args[[1]] <- NULL
  } else if(class == "ps"){
    args <- object$parameters
    args[[1]] <- NULL
  } else args <- NULL
  as.list(args)
}

# get formula from a fitted object and its class
get_formula <- function(object, class){

  if(class %in% c("lm", "glm", "gbm", "ps")){
    form <- eval(get_args(object, class)[["formula"]])
  } else if(class %in% c("pbart", "wbart")){
    x <- attr(object$varcount.mean, "names")
    form <- as.formula(paste(y, " ~ ", paste(x, collapse= "+")))
  } else return(NULL)
  environment(form) <- parent.frame()
  form
}

# get model frame except Y from formula and data
mframe <- function(formula, data){

  mf <- model.frame(formula, data, na.action = NULL)
  mf[, -1, drop = FALSE]
}

# fit a model given class, args, and newdata
fit <- function(class, formula, args, newdata){
  if(class %in% c("pbart", "wbart")){
    X <- as.matrix(mframe(formula, data = newdata))
    Y <- model.frame(formula, data = newdata, na.action = NULL)[[1]]
    args <- list(x.train = X, y.train = Y)
    sink(tempfile()); on.exit(sink(), add = TRUE)
    do.call(get(class), args)
  } else {
    args$data <- newdata
    sink(tempfile()); on.exit(sink(), add = TRUE)
    do.call(get(class), args)
  }
}


impute <- function(model, mf){

  mf1_untreated <- mf[!treated, , drop = FALSE]; mf1_untreated[, a] <- 1
  mf0_treated <- mf[treated, , drop = FALSE]; mf0_treated[, a] <- 0

  sink(tempfile()); on.exit(sink(), add = TRUE)
  imp_y1_untreated <- pred(model, mf1_untreated)
  imp_y0_treated <- pred(model, mf0_treated)

  list(imp_y1_untreated, imp_y0_treated)
}

pure <- function(imp, class, args, family){

  imp_y1_untreated <- imp[[1]]
  imp_y0_treated <- imp[[2]]

  if(class %in% c("pbart", "wbart")){

    args_imp_y1 <- list(x.train = as.matrix(X0),
                        y.train = imp_y1_untreated,
                        x.test = as.matrix(X))
    args_imp_y0 <- list(x.train = as.matrix(X1),
                        y.train = imp_y0_treated,
                        x.test = as.matrix(X))

    sink(tempfile()); on.exit(sink(), add = TRUE)
    model_imp_y1 <- do.call("wbart", args_imp_y1)
    model_imp_y0 <- do.call("wbart", args_imp_y0)

    imp_y1 <- model_imp_y1[["yhat.test.mean"]]
    imp_y0 <- model_imp_y0[["yhat.test.mean"]]

  } else{

    args_imp_y0 <- args_imp_y1 <- args

    args_imp_y1$formula <- as.formula(paste("imp_y1_untreated", " ~ ",
                                            paste(x, collapse= "+")))
    args_imp_y0$formula <- as.formula(paste("imp_y0_treated", " ~ ",
                                            paste(x, collapse= "+")))

    if(class == "gbm"){
      X0$imp_y1_untreated <- imp_y1_untreated
      X1$imp_y0_treated <- imp_y0_treated
      args_imp_y1$distribution <- args_imp_y0$distribution <- "gaussian"

    } else if(family[["family"]] %in% c("binomial", "poisson")){
      args_imp_y1$family <- args_imp_y0$family <- paste0("quasi", family[["family"]])
    }

    args_imp_y1$data <- X0
    args_imp_y0$data <- X1

    model_imp_y1 <- do.call(get(class), args_imp_y1)
    model_imp_y0 <- do.call(get(class), args_imp_y0)

    imp_y1 <- pred(model_imp_y1, X)
    imp_y0 <- pred(model_imp_y0, X)
  }

  imp_Ey1 <- mean(imp_y1, na.rm = TRUE)
  imp_Ey0 <- mean(imp_y0, na.rm = TRUE)

  c(imp_Ey1, imp_Ey0)
}

hybrid <- function(imp){

  imp_y1_untreated <- imp[[1]]
  imp_y0_treated <- imp[[2]]

  imp_Ey1 <- sum(imp_y1_untreated * ipw[!treated], na.rm = TRUE)/sum(ipw[!treated], na.rm = TRUE)
  imp_Ey0 <- sum(imp_y0_treated * ipw[treated], na.rm = TRUE)/sum(ipw[treated], na.rm = TRUE)

  c(imp_Ey1, imp_Ey0)
}

