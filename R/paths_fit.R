#####################################################
# Internal function to refit the model given formulas
#####################################################
paths_fit <- function(data, index = 1:nrow(data), varnames,
                      formulas, classes, families, args,
                      ps_formula, ps_class, ps_family, ps_args){

  newdata <- data[index, , drop = FALSE]

  # set environments for utility functions
  environment(get_args) <- environment(get_formula) <-
    environment(fit) <- environment(impute) <-
    environment(pure) <- environment(hybrid) <- environment()

  # fit K+1 outcome models
  fit_partial <- pryr::partial(fit, newdata = newdata)
  models <- Map(fit_partial, classes, formulas, args)
  cat(".")

  # fit ps_model
  if (!is.null(ps_class)){
    ps_model <- fit_partial(ps_class, ps_formula, ps_args)
  } else ps_model <- NULL

  # extract variable names
  a <- varnames$a
  x <- varnames$x
  y <- varnames$y

  # treatment indicator
  treated <- as.logical(newdata[[a]])

  # proportion of units treated
  prop_treated <- mean(treated)
  if(prop_treated==0 || prop_treated==1){
    warning("There must be both treated and untreated units.")
    return(NULL)
  }

  # use K to denote the number of mediators
  K <- length(models) - 1

  # extract model frames
  mfs <- lapply(formulas, mframe, data = newdata)

  # create design matrices for the covariates
  X <- newdata[, x, drop = FALSE]
  X0 <- X[!treated, , drop = FALSE]
  X1 <- X[treated, , drop = FALSE]

  # imputation for the baseline model
  A0 <- A1 <- mfs[[1]]
  A0[, a] <- 0; A1[, a] <- 1

  sink(tempfile())
  imp_y0 <- pred(object = models[[1]], newdata = A0)
  imp_y1 <- pred(object = models[[1]], newdata = A1)
  sink()

  imp_Ey0 <- mean(imp_y0, na.rm = TRUE)
  imp_Ey1 <- mean(imp_y1, na.rm = TRUE)

  # imputation for the outcome models
  imps <- Map(impute, models[(K+1):2], mfs[(K+1):2])

  # output matrices
  pure_out <- hybrid_out <- matrix(NA, nrow = K+2, ncol = 2)

  # pure imputation estimator
  pure_imps <- Map(pure, imps, classes[(K+1):2], args[(K+1):2], families[(K+1):2])
  pure_type1 <- c(imp_Ey0, vapply(pure_imps, `[[`, numeric(1), 1), imp_Ey1)
  pure_type2 <- c(imp_Ey1, vapply(pure_imps, `[[`, numeric(1), 2), imp_Ey0)
  pure_type1_decomp <- c(diff(pure_type1), imp_Ey1 - imp_Ey0)
  pure_type2_decomp <- c(-diff(pure_type2), imp_Ey1 - imp_Ey0)
  pure_out[] <- cbind(pure_type1_decomp, pure_type2_decomp)

  # hybrid estimator
  if(!is.null(ps_model)){

    # get pscore and ipw

    if(ps_class == "ps"){
      pscore <- ps_model$ps[[1]]
    } else pscore <- pred(ps_model, newdata = newdata)
    ipw <- prop_treated * treated / pscore + (1 - prop_treated) * (1-treated) / (1-pscore)

    # imputation-based weighting (hybrid) estimator
    hybrid_imps <- lapply(imps, hybrid)
    hybrid_type1 <- c(imp_Ey0, vapply(hybrid_imps, function(x) x[[1]], numeric(1)), imp_Ey1)
    hybrid_type2 <- c(imp_Ey1, vapply(hybrid_imps, function(x) x[[2]], numeric(1)), imp_Ey0)
    hybrid_type1_decomp <- c(diff(hybrid_type1), imp_Ey1 - imp_Ey0)
    hybrid_type2_decomp <- c(-diff(hybrid_type2), imp_Ey1 - imp_Ey0)
    hybrid_out[] <- cbind(hybrid_type1_decomp, hybrid_type2_decomp)
  }

  measure <- c("pure_Type I", "pure_Type II", "hybrid_Type I", "hybrid_Type II")
  path <- c("direct", paste0("via M", K:1), "total")

  out <- setNames(c(pure_out, hybrid_out),
                  paste(rep(measure, each = K+2), path, sep = "_"))
  out

}
