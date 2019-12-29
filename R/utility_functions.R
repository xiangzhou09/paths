#### Internal function to determine model type
model_type <- function(model) {

  lm_names <- "lm"
  glm_names <- "glm"
  bart_names <- c("abart", "gbart", "lbart",
                  "pbart", "mbart", "mbart2",
                  "recurbart", "survbart",
                  "wbart")

  type <- character(length = length(model))
  type[model %in% lm_names] <- "lm"
  type[model %in% glm_names] <- "glm"
  type[model %in% bart_names] <- "BART"

  if(!all(type %in% c("lm", "glm", "BART"))) {
    stop("One or more model has undeterminable names.")
  }

  return(type)

}

#### Internal function to refit the model given formulas

model_fit <- function(data, formulas, models, models_args = NULL) {

  ## Input check for when model_fit is called independently outside of paths

  # Check if formula(s) are provided correctly
  if(is.null(formulas)|!inherits(formulas, "list")) {
    stop("'formulas' must be supplied as a list of model formulas")
  }

  n_models <- length(formulas)
  K <- n_models - 1

  n_models <- length(formulas)

  # Check if model types are provided correctly
  if(is.null(models)) {
    stop("'models' must be supplied as a vector or a list of model types")
  } else if(inherits(models, "list")) {
    models <- unlist(models)
  }
  if(length(models) != n_models) {
      stop("Arguments 'formulas' and 'models' must be of equal lengths")
  }

  # Check if model argument(s) are provided correctly
  if(is.null(models_args) | missing(models_args)){
    message("Argument 'models_args' is not supplied, using default specifications for all models")
    models_args <- rep(NULL, length(formulas))

  } else {

    if(!inherits(models_args, "list") | !all(sapply(models_args, function(arg) inherits(arg, "list")|is.null(arg)))) {
      stop("Argument 'models_args' must be a list containing lists of arguments or NULL")
    } else if (any(sapply(models_args, function(x) any(sapply(x, is.null))))) {
      # prevent pbart from crashing if NULL is submitted as an argument
      stop("No list of arguments within 'models_args' may contain NULL")
    }

    if(length(models_args) != n_models) {
      stop("Arguments 'formulas' and 'models_args' must be of equal lengths")
    }
  }


  ## Fit a model for each of the input formulas ##
  model_objects <- vector("list", n_models)

  for(i in 1:n_models) {
    # Fit one model object for each formula
    if(model_type(models[i]) == "lm") {
      args <- c(list(formula = formulas[[i]],
                     data = data),
                models_args[[i]])

      model_objects[[i]] <- do.call(lm, args)

    } else if(model_type(models[i]) == "glm") {
      args <- c(list(formula = formulas[[i]],
                     data = data),
                models_args[[i]])

      model_objects[[i]] <- do.call(glm, args)

    } else if(model_type(models[i]) == "BART") {
      # bart does not automatically convert flexibly named
      # variables in formulas e.g. log(X)
      x.train <- model.matrix(formulas[[i]], data)[,-1]
      y.train <- model.frame(formulas[[i]], data)[,1]

      args <- c(list(x.train = x.train,
                     y.train = y.train),
                models_args[[i]],
                list(printevery = 2000))

      sink(tempfile())
      if(models[i] == "wbart") {
        model_objects[[i]] <- do.call(BART::wbart, args)
      } else if(models[i] == "pbart") {
        model_objects[[i]] <- do.call(BART::pbart, args)
      } else if(models[i] == "lbart") {
        model_objects[[i]] <- do.call(BART::lbart, args)
      }
      sink()

    }

  }

  return(model_objects)
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
