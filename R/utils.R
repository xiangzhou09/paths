utils::globalVariables(c("a", "x", "y", "X", "X0", "X1", "ipw", "treated"))

impute <- function(model, mf){

  mf1_untreated <- mf[!treated, , drop = FALSE]; mf1_untreated[, a] <- 1
  mf0_treated <- mf[treated, , drop = FALSE]; mf0_treated[, a] <- 0

  imp_y1_untreated <- pred(model, mf1_untreated)
  imp_y0_treated <- pred(model, mf0_treated)

  list(imp_y1_untreated, imp_y0_treated)
}

pure <- function(imp, class, family){

  imp_y1_untreated <- imp[[1L]]
  imp_y0_treated <- imp[[2L]]

  form_imp_y1 <- as.formula(paste("imp_y1_untreated", " ~ ", paste(x, collapse= "+")))
  form_imp_y0 <- as.formula(paste("imp_y0_treated", " ~ ", paste(x, collapse= "+")))

  if(class == "lm"){

    model_imp_y1 <- lm(form_imp_y1, data = X0)
    model_imp_y0 <- lm(form_imp_y0, data = X1)

    imp_y1 <- pred(model_imp_y1, X)
    imp_y0 <- pred(model_imp_y0, X)

  } else if(class == "glm"){

    model_imp_y1 <- glm(form_imp_y1, family = family, data = X0)
    model_imp_y0 <- glm(form_imp_y0, family = family, data = X1)

    imp_y1 <- pred(model_imp_y1, X)
    imp_y0 <- pred(model_imp_y0, X)

  } else if(class %in% c("pbart", "wbart")){

    model_imp_y1 <- wbart(x.train = as.matrix(X0),
                          y.train = imp_y1_untreated,
                          x.test = as.matrix(X))
    model_imp_y0 <- wbart(x.train = as.matrix(X1),
                          y.train = imp_y0_treated,
                          x.test = as.matrix(X))
    imp_y1 <- model_imp_y1[["yhat.test.mean"]]
    imp_y0 <- model_imp_y0[["yhat.test.mean"]]

  } else stop("'class' must be one of 'lm', 'glm', 'pbart', or 'wbart'")

  imp_Ey1 <- mean(imp_y1, na.rm = TRUE)
  imp_Ey0 <- mean(imp_y0, na.rm = TRUE)

  c(imp_Ey1, imp_Ey0)
}

hybrid <- function(imp){

  imp_y1_untreated <- imp[[1L]]
  imp_y0_treated <- imp[[2L]]

  imp_Ey1 <- sum(imp_y1_untreated * ipw[!treated], na.rm = TRUE)/sum(ipw[!treated], na.rm = TRUE)
  imp_Ey0 <- sum(imp_y0_treated * ipw[treated], na.rm = TRUE)/sum(ipw[treated], na.rm = TRUE)

  c(imp_Ey1, imp_Ey0)
}
