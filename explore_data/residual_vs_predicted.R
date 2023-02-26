# R function to plot residuals agianst predicted

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


## Fit a linear model
# lm_object <- lm(mpg ~ disp, data = mtcars)

## Create residual plot
# residual_plot(lm_object)
