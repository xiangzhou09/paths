######################################################
# Plot method for sens objects
######################################################
#'
#' Plot Method for \code{sens} Objects
#'
#' Contour plot for sensitivity analysis objects returned by \code{\link{sens}}.
#'
#' @param x object of class \code{sens} as produced by the \code{\link{sens}}
#'   function
#' @param outcome_name a character string indicating the name of the outcome.
#' @param x_axis sensitivity analysis parameter shown on the x axis. Default is "eta_k",
#'   i.e., the difference in the prevalence of the unobserved confounder \eqn{U} between treated
#'   and untreated units given pretreatment covariates \eqn{X} and mediators \eqn{M_1,\ldots, M_k}.
#'   Alternatively, it can be "gamma_k", i.e., the average effect of the unobserved confounder
#'   \eqn{U} on the outcome given pretreatment covariates \eqn{X}, treatment \eqn{A},
#'   and mediators \eqn{M_1,\ldots, M_k}.
#' @param other a named list indicating the values at which other sensitivity analysis parameters,
#'   namely, \code{`eta_k-1`} and \code{`gamma_k-1`}, are held. This is needed only when the
#'   bias formula involves \eqn{\eta_{k-1}} and \eqn{\gamma_{k-1}} as well as \eqn{\eta_k} and
#'   \eqn{\gamma_k}. The default is to set both \eqn{\eta_{k-1}} and \eqn{\gamma_{k-1}} at their
#'   average values in the \code{sens} object.
#' @param ... additional arguments
#'
#' @return a \code{ggplot2} plot, which can be further customized by the user.
#'
#' @import ggplot2
#'
#' @example inst/examples/sens.paths-example.R
#'
#' @export
plot.sens <- function(x, outcome_name = "Outcome", x_axis = c("eta_k", "gamma_k"),
                      other = list(`eta_k-1` = NULL, `gamma_k-1` = NULL), ...) {

  adjusted <- x$adjusted
  original <- x$original
  k <- x$k

  x_axis <- match.arg(x_axis)
  y_axis <- setdiff(c("eta_k", "gamma_k"), x_axis)

  if("eta_k-1" %in% names(adjusted)){

    if(length(other$`eta_k-1`)>1 || length(other$`gamma_k-1`)>1)
      stop("'eta_k-1' and 'gamma_k-1' in 'other' should either be NULL or a scalar.")

    adjusted[["eta_k-1"]] <- other[["eta_k-1"]] %||% mean(adjusted[["eta_k-1"]], na.rm = TRUE)
    adjusted[["gamma_k-1"]] <- other[["gamma_k-1"]] %||% mean(adjusted[["gamma_k-1"]], na.rm = TRUE)

    dup <- duplicated(adjusted[, c("eta_k-1", "gamma_k-1", "eta_k", "gamma_k")])
    adjusted <- adjusted[!dup]

    adjusted$bias <- with(adjusted, `gamma_k-1` * `eta_k-1` - `gamma_k` * `eta_k`)

    adjusted$value <- original$estimate - adjusted$bias
    adjusted$lower <- original$lower - adjusted$bias
    adjusted$upper <- original$upper - adjusted$bias
    adjusted$reversed <- (sign(adjusted$value) != sign(original$estimate))
  }

  xlabel <- bquote("Difference in the Prevalence of U between Treated and Untreated Units"~(eta[.(k)]))
  ylabel <- bquote("Effect of U on"~.(outcome_name)~(gamma[.(k)]))

  if(x_axis == "gamma_k") {tmp <- xlabel; xlabel <- ylabel; ylabel <- tmp}

  ggplot(data = adjusted, aes_(as.name(x_axis), as.name(y_axis))) +
    geom_raster(aes_(fill = quote(reversed)), show.legend = FALSE, na.rm = TRUE) +
    scale_fill_manual(values = c(NA, "grey70")) +
    geom_contour(aes_(z = quote(value)), colour = "black") +
    metR::geom_text_contour(aes_(z = quote(value)), size = 5) +
    labs(x = xlabel, y = ylabel) +
    theme_minimal(base_size = 14)
}
