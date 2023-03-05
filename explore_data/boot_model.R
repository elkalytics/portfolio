#' Function to obtain bootstrapped confidence intervals for a linear model
#' 
#' The user can specify desired parameters to bootstrap confidence intervals for a linear model.
#' The output returned is a list of results along with histograms showing the sampling
#' distribution of the intercepts and slopes.
#' 
#' @param data A data.frame containing the data to fit the model.
#' @param formula A formula specifying the linear model to be fit.
#' @param R An integer specifying the number of bootstrap replicates.
#' @param type A character string specifying the type of bootstrap confidence interval to compute.
#' 
#' @return A list containing the bootstrap confidence intervals for the intercept and slope of the linear model.
#' 
#' @importFrom boot boot boot.ci
#' 
#' @examples
#' boot_model(mtcars, "mpg ~ wt + qsec")
#'
#' @export
boot_model <- function(data, formula, R = 1000, type = "basic") {
  
  # Load library
  requireNamespace("boot", quietly = TRUE)
  
  # Define function to compute statistic
  boot_fn <- function(data, index) {
    fit <- lm(formula, data = data[index,])
    coef(fit)
  }
  
  # Generate bootstrap samples
  set.seed(123) # for reproducibility
  boot_results <- boot(data, boot_fn, R = R)
  
  # Compute confidence intervals using the specified bootstrap method
  boot_ci <- boot.ci(boot_results, type = type)
  
  # Plot histogram of bootstrap sampling distribution of coefficients
  par(mfrow = c(1,2))
  hist(boot_results$t[,1], main = "Histogram of Bootstrap Sampling Distribution of Intercept")
  hist(boot_results$t[,2], main = "Histogram of Bootstrap Sampling Distribution of Slope")
  
  # Return the bootstrap confidence intervals for the coefficients
  return(boot_ci)
}