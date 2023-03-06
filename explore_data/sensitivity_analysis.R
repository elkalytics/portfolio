#' Perform sensitivity analysis
#'
#' This function performs sensitivity analysis on a dataset by simulating the portfolio losses for 
#' different percentile thresholds. It uses parallel computing to speed up the computation.
#'
#' @param data A data.frame that contains the value and losses for the portfolio.
#' @param threshold_seq A numeric vector of percentile thresholds to use in the sensitivity analysis.
#' @param num_sims An integer indicating the number of simulations to perform for each percentile threshold.
#'
#' @return A data.frame with columns for the percentile threshold and the variance of the simulated losses.
#'
#' @import doParallel
#' @export
#' @examples
#' # Generate example data
#' set.seed(123)
#' value <- rnorm(1000, mean = 1000, sd = 100)
#' losses <- -value * rnorm(1000, mean = 0.05, sd = 0.1)
#' data <- data.frame(value, losses)
#'
#' # Perform sensitivity analysis on example data
#' threshold_seq <- seq(0.01, 0.1, by = 0.01)
#' num_sims <- 1000
#' sensitivity_results <- sensitivity_analysis(data, threshold_seq, num_sims)
#'
#' # View sensitivity analysis results
#' head(sensitivity_results)
sensitivity_analysis <- function(data, threshold_seq, num_sims) {
  # Set up parallel computing
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  # Precompute quantiles
  quantiles <- quantile(data$value, threshold_seq)
  
  # Define function to calculate portfolio losses for given percentile threshold
  calculate_losses <- function(data, quantiles, threshold) {
    data$losses[data$value < quantiles[threshold]]
  }
  
  # Perform sensitivity analysis
  sim_losses <- foreach(threshold = threshold_seq, .combine = "cbind") %dopar% {
    replicate(num_sims, sum(calculate_losses(data, quantiles, threshold)))
  }
  sim_variances <- apply(sim_losses, 1, var)
  
  # Clean up parallel computing
  stopCluster(cl)
  registerDoSEQ()
  
  # Return sensitivity analysis results
  sensitivity_results <- data.frame(threshold = threshold_seq, variance = sim_variances)
  return(sensitivity_results)
}
