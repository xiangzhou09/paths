######################################################
# Plot method for paths objects
######################################################
#'
#' Plot Method for \code{paths} Objects
#'
#' Plot point estimates and confidence intervals for each individual
#' path-specific effect from a \code{paths} object.
#'
#' @param x an object of class \code{paths} returned by the \code{\link{paths}}
#'   function
#'
#' @param mediator_names a vector of character strings giving the labels for each
#'   mediator. It must contain as many elements as the number of mediators in the
#'   model. If not supplied, a set of default labels will be constructed from the
#'   fitted \code{paths} object.
#'
#' @param estimator either \code{"pure"}, \code{"hybrid"}, or \code{"both"},
#'   indicating whether the plot will display estimates obtained using the pure imputation
#'   estimator, the imputation-based weighting estimator, or both. Default is to show
#'   estimates from the pure imputation estimator.
#'
#' @param decomp either \code{"Type I"}, \code{"Type II"}, or \code{"both"},
#'   indicating whether the plot will display estimates obtained using Type I decomposition,
#'   Type II decomposition, or both Type I and Type II decompositions. Default is to show
#'   estimates from Type I decomposition.
#'
#' @param horizontal a logical variable indicating whether a horizontal plot should be
#'   used. Default is to use a horizontal plot when \code{decomp != both}.
#'
#' @param ... additional arguments.
#'
#' @return a \code{ggplot2} plot, which can be further customized by the user.
#'
#' @rdname plot.paths
#' @import ggplot2
#' @example inst/examples/plot.paths-example.R
#'
#' @export
plot.paths <- function(x, mediator_names = NULL,
                       estimator = c("pure", "hybrid", "both"),
                       decomp = c("Type I", "Type II", "both"),
                       horizontal = (decomp != "both"), ...) {

  # number of mediators
  K <- length(x$varnames$m)

  # check mediator names
  if(is.null(mediator_names)){
    mediators <- vapply(x$varnames$m, function(m) paste(m, collapse = " + "), character(1))
  } else {
    if(length(mediator_names) != K) {
      stop("'mediator_names' must have the same length as the number of mediators")
    }
    mediators <- mediator_names
  }

  # match decomp and estimator arguments
  decomp <- match.arg(decomp)
  estimator <- match.arg(estimator)

  # check estimator
  if((estimator != "pure") && is.null(x$ps_formula)) {
    warning("Estimates using the imputation-based weighting (hybrid) estimator are not available;
             Using the pure imputation estimator instead")
    estimator <- "pure"
  }
  if(estimator == "both") estimator <- c("pure", "hybrid")

  # create plot data
  plot_data <- do.call("rbind", c(x[estimator], make.row.names = FALSE))

  # label estimands
  estimand_levs <- c(paste0("via ", "M", K:1), "direct", "total")
  estimand_labs <- c(paste("via", rev(mediators)), "Direct Effect", "Total Effect")
  plot_data$estimand <- factor(plot_data$estimand, levels = estimand_levs, labels = estimand_labs)

  # label estimator
  if(length(estimator) == 2){
    plot_data$estimator <- factor(plot_data$estimator,
                                  levels = c("pure", "hybrid"),
                                  labels = c("Pure Imputation Estimator",
                                             "Imputation-Based Weighting Estimator"))

    rmv <- (plot_data$estimand == "Total Effect") &
      (plot_data$estimator == "Imputation-Based Weighting Estimator")

    plot_data <- plot_data[!rmv, , drop = FALSE]

    # create color and shape scale for ggplot
    color_scale <- scale_color_discrete(name = "")
    shape_scale <- scale_shape_discrete(name = "")

  } else{

    plot_data$estimator <- if(estimator == "pure") "Pure Imputation Estimator" else{
      "Imputation-Based Weighting Estimator"
    }

    # create color and shape scale for ggplot
    color_scale <- scale_color_manual(guide = FALSE, values = "black")
    shape_scale <- scale_shape_discrete(guide = FALSE)
  }

  # label decomposition
  if(decomp == "both"){
    plot_data$decomposition <- factor(plot_data$decomposition,
                                      levels = c("Type I", "Type II"),
                                      labels = c("Type I Decomposition",
                                                 "Type II Decomposition"))

    # create facet for ggplot
    decomp_facet <- facet_wrap(. ~ decomposition)
  } else{
    plot_data <- plot_data[plot_data$decomposition == decomp, ]
    plot_data$decomposition <- paste0(decomp, " Decomposition")

    decomp_facet <- NULL
  }

  if(horizontal){
    coord_horiz <- coord_flip()
  } else{
    coord_horiz <- NULL
    plot_data$estimand <- factor(plot_data$estimand,
                                 levels = rev(levels(plot_data$estimand)))
  }

  # Generate plot
  ggplot(plot_data, aes_(x = quote(estimand), y = quote(estimate),
                         shape = quote(estimator), colour = quote(estimator))) +
    geom_pointrange(aes_(ymin = quote(lower), ymax = quote(upper)),
                    size = 1, position = position_dodge2(width = .5)) +
    xlab("") +
    ylab("Estimates of Total and Path-Specific Effects") +
    color_scale +
    shape_scale +
    decomp_facet +
    coord_horiz +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}
