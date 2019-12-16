#### workhorse function to generate point estimates
paths_fun <- function(data, index = 1:nrow(data),
                      formulas, models, models_args,
                      treat, outcome,
                      conditional,
                      ps,
                      ps_formula, ps_model, ps_model_args) {

  n_models <- length(formulas)
  K <- n_models - 1

  # extract vectors of outcomes, of all variables, and of treatment
  x <- data[index, ]

  y <- x[[outcome]]
  a <- x[[treat]]==1

  n <- nrow(x)

  # Adding weights, if needed
  if(ps) {

    # fit propensity score model and extract weights
    ps_mod <- model_fit(x, ps_formula, ps_model, ps_model_args)[[1]]

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
  model_objects <- model_fit(x, formulas, models, models_args)

  # Total effect
  if(conditional) {
    # for observational studies, TE must be calculated
    # conditional on X
    x_te <- x

    if(model_type(models[n_models]) %in% c("lm", "glm")) {

      x_te[,treat] <- 1
      E_a1 <- mean(predict(model_objects[[n_models]], newdata = data.frame(x_te)))

      x_te[,treat] <- 0
      E_a0 <- mean(predict(model_objects[[n_models]], newdata = data.frame(x_te)))

    } else if(model_type(models[n_models]) == "BART") {

      if(models[n_models] %in% c("pbart", "lbart")) {

        sink(tempfile())

        x_te[,treat] <- 1
        mat_x_te <- model.matrix(formulas[[n_models]], x_te)[,colnames(model_objects[[n_models]]$varcount)]
        E_a1 <- mean(predict(model_objects[[n_models]], newdata = mat_x_te)[["prob.test.mean"]])

        x_te[,treat] <- 0
        mat_x_te <- model.matrix(formulas[[n_models]], x_te)[,colnames(model_objects[[n_models]]$varcount)]
        E_a0 <- mean(predict(model_objects[[n_models]], newdata = mat_x_te)[["prob.test.mean"]])

        sink()

      } else if(models[n_models] == c("wbart")) {

        sink(tempfile())

        x_te[,treat] <- 1
        mat_x_te <- model.matrix(formulas[[n_models]], x_te)[,colnames(model_objects[[n_models]]$varcount)]
        E_a1 <- mean(predict(model_objects[[n_models]], newdata = mat_x_te, dodraws = FALSE))

        x_te[,treat] <- 0
        mat_x_te <- model.matrix(formulas[[n_models]], x_te)[,colnames(model_objects[[n_models]]$varcount)]
        E_a0 <- mean(predict(model_objects[[n_models]], newdata = mat_x_te, dodraws = FALSE))

        sink()

      } else {
        stop(paste("Model ", k," belongs to an unsupported BART family"))
      }


    }
  } else {
    # for experiments, TE can be calculated unconditionally
    E_a1 <- mean(y[a])
    E_a0 <- mean(y[!a])

  }

  # Decomposition Type 1
  x_a0 <- x[!a,]
  x_a0[,treat] <- 1
  n_a0 <- nrow(x_a0)

  # E[Y(1, M_k(0))] are estimated differently depending on whether the
  # setting is experimental (conditional = FALSE)
  # or observational (conditional = TRUE), and then on whether
  # the estimator is pure imputation (ps = FALSE) or imputation-
  # based (ps = TRUE)

  E_y_1_mk_0 <- sapply(K:1, function(k) {

    ## Impute counterfactual outcome for Y(1, M_k(0))
    if(model_type(models[k]) == "lm") {

      y_1_mk_0 <- predict(model_objects[[k]], newdata = x_a0)

    } else if(model_type(models[k]) == "glm") {

      y_1_mk_0 <- predict(model_objects[[k]], newdata = x_a0)

    } else if(model_type(models[k]) == "BART") {

      mat_x_a0 <- model.matrix(formulas[[k]], x_a0)[,colnames(model_objects[[k]]$varcount)]

      if (inherits(model_objects[[k]], c("pbart", "lbart"))) {
        sink(tempfile())
        y_1_mk_0 <- predict(model_objects[[k]], newdata = mat_x_a0)[["prob.test.mean"]]
        sink()
      } else if(inherits(model_objects[[k]], "wbart")) {
        sink(tempfile())
        y_1_mk_0 <- predict(model_objects[[k]], newdata = mat_x_a0, dodraws = FALSE)
        sink()
      } else {
        stop(paste("Model ", k," belongs to an unsupported BART family"))
      }
    }

    if(conditional & !ps) {
      ## When conditional = TRUE, and ps = FALSE,
      ## E[Y(1, M_k(0))] is calculated using
      ## average of predicted counterfactual from
      ## a model Y ~ X that follows the specification
      ## of the model Y ~ A + X (k = K)

      x_a0[[outcome]] <- y_1_mk_0
      formula_yhat <- update(formulas[[n_models]], paste(". ~ . -", treat))

      model_yhat <- models[n_models]
      model_args_yhat <- model_args[[n_models]]

      ## For Poisson and Binomial models, use quasi family to fit continuous outcomes
      if(model_type(models[n_models]) == "glm") {
        if(family(model_objects[[k]]) == "poisson"){
          model_args_yhat$family <- quasipoisson(link = model_args_yhat$family$link)
        } else if (family(model_objects[[k]]) == "binomial"){
          model_args_yhat$family <- quasibinomial(link = model_args_yhat$family$link)
        }
      }

      model_objects_yhat <- model_fit(x_a0,
                                      list(formula_yhat),
                                      model_yhat,
                                      list(model_args_yhat))[[1]]

      if(model_type(models[n_models]) %in% c("lm", "glm")) {

        y_1_mk_0 <- predict(model_objects_yhat, newdata = x)

      } else if(model_type(models[n_models]) == "BART") {

        mat_x <- model.matrix(formula_yhat, x)[,colnames(model_objects_yhat$varcount)]
        if(inherits(model_objects_yhat, c("pbart", "lbart"))) {
          sink(tempfile())
          y_1_mk_0 <- predict(model_objects_yhat, newdata = mat_x)[["prob.test.mean"]]
          sink()
        } else if(inherits(model_objects_yhat, "wbart"))  {
          sink(tempfile())
          y_1_mk_0 <- predict(model_objects_yhat, newdata = mat_x, dodraws = FALSE)
          sink()
        } else {
          stop(paste("Model ", n_models," belongs to an unsupported BART family"))
        }

      }

      mean(y_1_mk_0)

    } else {

      ## When conditional = TRUE, and ps = TRUE,
      ## E[Y(1, M_k(0))] is calculated using
      ## weighted average of imputed counterfactual

      ## When conditional = FALSE,
      ## E[Y(1, M_k(0))] is calculated using
      ## simple average of imputed counterfactual

      weighted.mean(y_1_mk_0, w = ipw_a0) # equal to simple average when ipw_a0 is constant

    }
  })

  # Decomposition Type 2
  x_a1 <- x[a,]
  x_a1[,treat] <- 0
  n_a1 <- nrow(x_a1)

  # E[Y(0, M_k(1))] are estimated differently depending on whether the
  # setting is experimental (conditional = FALSE)
  # or observational (conditional = TRUE), and then on whether
  # the estimator is pure imputation (ps = FALSE) or imputation-
  # based (ps = TRUE)

  E_y_0_mk_1 <- sapply(K:1, function(k) {

    ## Impute counterfactual outcome for Y(1, M_k(0))
    if(model_type(models[k]) == "lm") {

      y_0_mk_1 <- predict(model_objects[[k]], newdata = x_a1)

    } else if(model_type(models[k]) == "glm") {

      y_0_mk_1 <- predict(model_objects[[k]], newdata = x_a1)

    } else if(model_type(models[k]) == "BART") {

      mat_x_a1 <- model.matrix(formulas[[k]], x_a1)[,colnames(model_objects[[k]]$varcount)]

      if (inherits(model_objects[[k]], c("pbart", "lbart"))) {
        sink(tempfile())
        y_0_mk_1 <- predict(model_objects[[k]], newdata = mat_x_a1)[["prob.test.mean"]]
        sink()
      } else if(inherits(model_objects[[k]], "wbart")) {
        sink(tempfile())
        y_0_mk_1 <- predict(model_objects[[k]], newdata = mat_x_a1, dodraws = FALSE)
        sink()
      } else {
        stop(paste("Model ", k," belongs to an unsupported BART family"))
      }
    }

    if(conditional & !ps) {
      ## When conditional = TRUE, and ps = FALSE,
      ## E[Y(0, M_k(1))] is calculated using
      ## average of predicted counterfactual from
      ## a model Y ~ X that follows the specification
      ## of the model Y ~ A + X (k = K)


      x_a1[[outcome]] <- y_0_mk_1
      formula_yhat <- update(formulas[[n_models]], paste(". ~ . -", treat))

      model_yhat <- models[n_models]
      model_args_yhat <- model_args[[n_models]]

      ## For Poisson and Binomial models, use quasi family to fit continuous outcomes
      if(model_type(models[n_models]) == "glm") {
        if(family(model_objects[[k]]) == "poisson"){
          model_args_yhat$family <- quasipoisson(link = model_args_yhat$family$link)
        } else if (family(model_objects[[k]]) == "binomial"){
          model_args_yhat$family <- quasibinomial(link = model_args_yhat$family$link)
        }
      }

      model_objects_yhat <- model_fit(x_a0,
                                      list(formula_yhat),
                                      model_yhat,
                                      list(model_args_yhat))[[1]]


      if(model_type(models[n_models]) %in% c("lm", "glm")) {

        y_0_mk_1 <- predict(model_objects_yhat, newdata = x)

      } else if(model_type(models[n_models]) == "BART") {

        mat_x <- model.matrix(formula_yhat, x)[,colnames(model_yhat$varcount)]
        if(inherits(model_objects_yhat, c("pbart", "lbart"))) {
          sink(tempfile())
          y_0_mk_1 <- predict(model_objects_yhat, newdata = mat_x)[["prob.test.mean"]]
          sink()
        } else if(inherits(model_objects_yhat, "wbart"))  {
          sink(tempfile())
          y_0_mk_1 <- predict(model_objects_yhat, newdata = mat_x, dodraws = FALSE)
          sink()
        } else {
          stop(paste("Model ", n_models," belongs to an unsupported BART family"))
        }

      }

      mean(y_0_mk_1)

    } else {

      ## When conditional = TRUE, and ps = TRUE,
      ## E[Y(0, M_k(1))] is calculated using
      ## weighted average of imputed counterfactual

      ## When conditional = FALSE,
      ## E[Y(0, M_k(1))] is calculated using
      ## simple average of imputed counterfactual

      weighted.mean(y_0_mk_1, w = ipw_a1) # equal to simple average when ipw_a0 is constant

    }
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
