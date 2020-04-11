#' #####################################################
#' Plot method for paths#' objects
#' #####################################################
#'
#' Plot Method for \code{paths} Objects
#'
#' Plot point estimates and confidence intervals for each individual
#' path-specific effect from a \code{paths} object.
#'
#' @param paths_obj object of class \code{paths} as produced by the \code{paths}
#'   function
#'
#' @param mediator_names a vector of strings indicating the labels for each
#'   mediator. Must contain as many elements as there are mediators in the
#'   model. If not supplied, the default labels will be constructed from the
#'   model formulas supplied to the \code{paths} function.
#'
#' @param type either \code{1}, \code{2}, or \code{c(1,2)}, indicating whether
#'   the plot will display estimates obtained using Type I, Type II, or both
#'   Type I and Type II decompsition. Default is to show estimates from both
#'   types of decomposition.
#'
#' @param estimator either \code{"pure"}, \code{"hybrid"}, or \code{c("pure",
#'   "hybrid")}, indicating whether the plot will display estimates obtained
#'   using the pure imputation estimator, the imputation-based weighting
#'   estimator, or both. Default is to show estimates from both estimators.
#'
#' @param horizontal logical indicating whether a horizontal plot should be
#'   generated.
#'
#' @return a \code{ggplot2} plot, which can be further customized by the user.
#'
#' @inherit paths seealso
#' @inherit paths author
#'
#' @rdname plot.paths
#'
#' @export
plot.paths <- function(paths_obj, mediator_names = NULL, type = c(1,2),
                       estimator = c("pure", "hybrid"), horizontal = FALSE) {

  if(is.null(mediator_names)){
    mediators <- sapply(paths_obj$varnames$m, function(m) paste(m, collapse = " + "))
  } else {
    if(length(mediator_names) != length(paths_obj$varnames$m)) {
      stop("'mediator_names' must have the same length with 'mediators'")
    }
    mediators <- mediator_names
  }


  if(any(unique(type) %notin% c(1,2))) {
    stop("'type' must be either 1, 2, or c(1,2)")
  }
  type <- c("Type I", "Type II")[type]

  if(any(unique(estimator) %notin% c("pure", "hybrid"))) {
    stop("'estimator' must be either \"pure\", \"hybrid\", or c(\"pure\", \"hybrid\")")
  }
  if("hybrid" %in% estimator & is.null(paths_obj$ps_formula)) {
    warning("Estimates using imputation-based weighting estimator were not calculated.
            Plotting only estimates using pure imputation estimator.")
    estimator <- "pure"
  }

  plot_data <- rbind(paths_obj$pure, if(!is.null(paths_obj$ps_formula)) paths_obj$hybrid)

  # labelling (hardcoding for now)
  estimand_labs <- c("Direct Effect", "Total Effect", paste("via", mediators))
  plot_data$estimand_plot <- relevel(factor(plot_data$estimand, labels = estimand_labs),
                                     ref = "Total Effect")

  estimator_labs <- if("hybrid" %in% estimator) {
    c("Imputation-Based Weighting Estimator", "Pure Imputation Estimator")
  } else {
    "Pure Imputation Estimator"
  }
  plot_data$estimator_plot <- relevel(factor(plot_data$estimator, labels = estimator_labs),
                                      ref = "Pure Imputation Estimator")

  decomposition_labs <- c("Type 1 Decomposition", "Type 2 Decomposition")
  plot_data$decomposition_plot <- relevel(factor(plot_data$decomposition, labels = decomposition_labs),
                                          ref = "Type 1 Decomposition")

  plot_data <- plot_data[which(plot_data$estimator %in% estimator &
                                 plot_data$decomposition %in% type),]
  plot_data <- plot_data[-which(plot_data$decomposition == "Type II" &
                                 plot_data$estimand == "total"), ]

  # Adjust aesthetics based on how many graphs are requested
  type_legend <- if(length(type) == 2) {
    scale_color_manual(labels = c("Type 1 Decomposition", "Type 2 Decomposition"),
                       values = c("blue", "red"))
  } else {
    scale_color_manual(guide = FALSE, values = c("black"))
  }
  estimator_facet <- if(length(estimator) == 2) facet_wrap(. ~ estimator_plot)
  coord_horiz <- if(horizontal) coord_flip()

  # Generate plot
  ggplot(plot_data, aes(x = estimand_plot, y = estimate, colour = decomposition_plot)) +
    geom_pointrange(aes(ymin = lower, ymax = upper), size = 1,
                    position = position_dodge2(width = .5)) +
    geom_vline(xintercept = 0, linetype = 2) +
    xlab("") +
    ylab("Estimates of Total and Path-Specific Effects") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    type_legend +
    estimator_facet +
    coord_horiz

}
