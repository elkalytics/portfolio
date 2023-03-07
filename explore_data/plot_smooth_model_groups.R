#' Plot grouped data with smooth curves
#'
#' @param data A data frame containing the variables used in the plot.
#' @param x The variable used for the x-axis.
#' @param y The variable used for the y-axis.
#' @param color The variable used for color grouping.
#' @param facet The variable used for faceting.
#' @param method The smoothing method used.
#'
#' @return A ggplot object.
#'
#' @import ggplot2
#'
#' @examples
#' plot_smooth_group(mpg, "displ", "hwy", color = NULL, facet = "cyl", method = "lm")
#' plot_smooth_group(mpg, "displ", "hwy", color = NULL, facet = "cyl", method = "loess")
#' plot_smooth_group(mpg, "displ", "hwy", color = NULL, facet = "cyl", method = "gam")
#' plot_smooth_group(mpg, "displ", "hwy", color = NULL, facet = "cyl", method = "glm")
library(ggplot2)
plot_smooth_group <- function(data, x, y, color, facet, method) {
  ggplot(data, aes_string(x, y, color = color)) +
    geom_point() +
    geom_smooth(method = method, se = TRUE) +
    facet_grid(paste(". ~", facet))
}