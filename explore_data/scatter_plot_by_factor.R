#' Create a scatter plot of two variables, colored by a categorical variable
#'
#' This function creates a scatter plot using ggplot2 of two variables from a data frame, with the points colored by a third categorical variable. If a fourth categorical variable is specified, the plot will be faceted by that variable.
#'
#' @param data A data frame containing the variables to be plotted
#' @param x_var A string specifying the name of the variable to plot on the x-axis
#' @param y_var A string specifying the name of the variable to plot on the y-axis
#' @param categorical_var A string specifying the name of the categorical variable to use for coloring the points
#' @param facet_var (optional) A string specifying the name of the categorical variable to use for faceting the plot
#' 
#' @return A ggplot2 object containing the scatter plot
#'
#' @import ggplot2
#' @importFrom rlang !! sym
#' 
#' @examples
#' scatter_plot_by_factor(mtcars, "mpg", "disp", "cyl", "gear")
#' scatter_plot_by_factor(mtcars, "mpg", "disp", "cyl")
#' 
#' @export
scatter_plot_by_factor <- function(data, x_var, y_var, categorical_var, facet_var = NULL) {
  library(ggplot2)
  
  # Convert the categorical variables to factors
  data[[categorical_var]] <- as.factor(data[[categorical_var]])
  if (!is.null(facet_var)) {
    data[[facet_var]] <- as.factor(data[[facet_var]])
  }
  
  # Create the plot using ggplot2
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(as.name(categorical_var)))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab(names(data)[match(x_var, names(data))]) +
    ylab(names(data)[match(y_var, names(data))]) +
    ggtitle(paste("Scatter of", names(data)[match(x_var, names(data))], "vs", names(data)[match(y_var, names(data))], "by", categorical_var)) +
    theme_minimal()
  
  # Add facet grid if necessary
  if (!is.null(facet_var)) {
    p <- p + facet_grid(as.formula(paste(".~", facet_var)))
  }
  
  return(p)
}