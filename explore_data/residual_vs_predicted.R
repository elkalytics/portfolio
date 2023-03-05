#' Plot residuals against predicted values
#'
#' This function creates a scatterplot of residuals against predicted values from a linear regression model.
#'
#' @param lm_object A linear model object.
#' @param point_color Color of the points in the scatterplot (default is "black").
#' @param fit_color Color of the line of best fit (default is "blue").
#' @param res_color Color of the lines connecting the residuals to the line of best fit (default is "red").
#' @param decimal_places Number of decimal places to display in the title of the plot (default is 2).
#'
#' @return A ggplot object of the residual plot.
#'
#' @examples
#' lm_object <- lm(mpg ~ disp, data = mtcars)
#' residual_plot(lm_object)
#'
#' @import ggplot2
#' @importFrom stats lm
#' @export
# Load package
library(ggplot2)

# Save function
residual_plot <- function(lm_object, point_color = "black", fit_color = "blue",
                          res_color = "red", decimal_places = 2) {
  
  # Check that the object is a linear model and has a formula and data
  if (!inherits(lm_object, "lm") ||
      is.null(attr(lm_object$terms, "dataClasses")) ||
      is.null(attr(lm_object$terms, "data"))) {
    stop("Input must be a linear model with formula and data")
  }
  
  # Extract predicted values and residuals
  pred <- lm_object$fitted.values
  res <- lm_object$residuals
  
  # Create data frame with predicted and residual values
  df <- data.frame(pred, res)
  
  # Calculate line of best fit
  fit <- lm(res ~ pred, data = df)
  r_squared <- summary(lm_object)$r.squared
  n_obs <- length(lm_object$residuals)
  
  # Create scatterplot with red lines for residuals
  p <- ggplot(df, aes(x = pred, y = res)) +
    geom_point(color = point_color) +
    geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2],
                color = fit_color) +
    geom_segment(aes(xend = pred, yend = fit$fitted.values),
                 color = res_color) +
    labs(x = "Predicted values", y = "Residuals", 
         title = paste("Residual plot (R² =", round(r_squared, decimal_places), 
                       "| n =", n_obs, ")"))
  
  return(p)
}