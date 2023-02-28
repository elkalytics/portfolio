# Function to obtain bootstrapped confidence intervals for model

# Save function
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


## Example use
# boot_model(mtcars, "mpg ~ wt + qsec")

