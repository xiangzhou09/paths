#' Extracting Model Frame
#'
#' Extracing model frame from lm, glm, pbart, and wbart objects.
#'
#' @param object fitted model object
#' @param data data set containing all variables
#'
#' @return model frame without the outcome variable.
#'
#' @export
mframe <- function(object, data, ...) UseMethod("mframe")

#' @export
#' @rdname mframe
mframe.default <- function(object, data){
  stop("currently no mframe.default method exists for the input object.")
}

#' @export
#' @rdname mframe
mframe.lm <- function(object, data = NULL){
  tmp <- model.frame(object, data = data, na.action = NULL)
  tmp[, -1L, drop = FALSE]
}

#' @export
#' @rdname mframe
mframe.glm <- function(object, data = NULL){
  tmp <- model.frame(object, data = data, na.action = NULL)
  tmp[, -1L, drop = FALSE]
}

#' @export
#' @rdname mframe
mframe.pbart <- function(object, data){
  x <- attr(object$varcount.mean, "names")
  out <- data[, x, drop = FALSE]
  class(out) <- "data.frame"
  out
}

#' @export
#' @rdname mframe
mframe.wbart <- function(object, data){
  x <- attr(object$varcount.mean, "names")
  out <- data[, x, drop = FALSE]
  class(out) <- "data.frame"
  out
}
