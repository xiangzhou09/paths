#####################################################
# Prediction methods
#####################################################
#' Obtaining predicted values from fitted models
#'
#' Generic function that returns expected outcomes from \code{lm}, \code{glm}, \code{wbart},
#'  and \code{bbart} objects with new data
#'
#' @param object fitted model object, which can be of class \code{lm}, \code{glm}, \code{wbart},
#'  or \code{pbart}.
#' @param newdata a data frame containing predictor variables.
#' @param ... additional arguments passed to the \code{predict} methods.
#'
#' @return a vector of expected outcomes for \code{newdata}
#'
#' @export
pred <- function(object, newdata, ...) UseMethod("pred")

#' @export
#' @rdname pred
pred.lm <- function(object, newdata, ...){
  predict.lm(object, newdata, ...)
}

#' @export
#' @rdname pred
pred.glm <- function(object, newdata, ...){
  predict.glm(object, newdata, type = "response", ...)
}

#' @export
#' @rdname pred
pred.pbart <- function(object, newdata, ...){
  predict(object, newdata, ...)[["prob.test.mean"]]
}

#' @export
#' @rdname pred
pred.wbart <- function(object, newdata, ...){
  colMeans(predict(object, newdata, ...), na.rm = TRUE)
}
