#' Rescaled Scatter Plot
#'
#' This function creates a scatter plot of two variables that have been rescaled so they have similar scales centered at 0.
#'
#' @param x a numeric vector of values for the x-axis
#' @param y a numeric vector of values for the y-axis
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' y <- x + rnorm(100, mean = 0, sd = 0.5)
#' plot_rescaled(x, y)
plot_rescaled <- function(x, y){
  library(ggplot2)
  
  # Rescale the variables so they have similar scales centered at 0
  x_rescaled <- (x - mean(x)) / sd(x)
  y_rescaled <- (y - mean(y)) / sd(y)
  
  # Create the scatter plot with a loess smooth
  ggplot(data.frame(x_rescaled, y_rescaled), aes(x_rescaled, y_rescaled)) +
    geom_point() +
    geom_smooth(method = "loess") +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_vline(xintercept = 0, linewidth = 1) +
    xlab("Rescaled x") +
    ylab("Rescaled y") +
    coord_equal()
}