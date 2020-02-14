#' Getting predicted values
#'
#' Getting predicted values from lm, glm, pbart, and wbart objects.
#'
#' @param object fitted model object
#' @param newdata a data frame containing predictors
#'
#' @return a vector of predicted values for \code{newdata}
#'
#' @export
pred <- function(object, newdata, ...) UseMethod("pred")

#' @export
pred.default <- function(object, newdata, ...){
  stop("currently no pred.default method exists for the input object.")
}

#' @export
#' @rdname pred
pred.lm <- function(object, newdata, ...){
  predict.lm(object, newdata, ...)
}

  predict.lm

#' @export
#' @rdname pred
pred.glm <- function(object, newdata, ...){
  predict.glm(object, newdata, type = "response", ...)
}

#' @export
#' @rdname pred
pred.pbart <- function(object, newdata, ...){
  predict(object, newdata)[["prob.test.mean"]]
}

#' @export
#' @rdname pred
pred.wbart <- function(object, newdata, ...){
  colMeans(predict(object, newdata), na.rm = TRUE)
}
