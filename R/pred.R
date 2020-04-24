#####################################################
# Prediction methods
#####################################################
#' Obtaining predicted values from fitted models
#'
#' Generic function that returns predicted outcomes from \code{lm}, \code{glm}, \code{gbm}, \code{wbart},
#'  and \code{pbart} objects with new data.
#'
#' @param object a fitted model object, which can be of class \code{lm}, \code{glm}, \code{gbm},
#'  \code{wbart}, or \code{pbart}.
#' @param newdata a data frame containing predictor variables.
#' @param method  Method used to determine the optimal number of boosting iterations for \code{gbm} objects.
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
pred.gbm <- function(object, newdata, method = "OOB", ...){
  best_iter <- suppressMessages(gbm::gbm.perf(object, method = method, plot.it = FALSE))
  gbm::predict.gbm(object, newdata, n.trees = best_iter, type = "response", ...)
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
