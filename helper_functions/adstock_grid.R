# Grid search function to be used with adstock function
# Searches for optimal parameters

# Load packages
library(dplyr)
library(doParallel)
library(foreach)
library(memoise)
library(future)
library(zoo)
library(furrr)

# Adstock
adstock <- function(x, lag, decay) {
  # Apply adstock transformation to a vector x
  n <- length(x)
  weights <- (1:lag) * decay
  weights <- weights / sum(weights)
  adstocked <- rep(0, n)
  for (i in lag:n) {
    adstocked[i] <- sum(x[(i-lag+1):i] * weights)
  }
  adstocked
}

# Define a function to calculate the sum of squared errors between the predicted and actual values
sse <- function(actual, predicted) {
  # Calculate the sum of squared errors between the predicted and actual values
  sum((actual - predicted)^2)
}

# Define a function to perform the grid search for optimal lag and decay values
adstock_grid_search <- function(x, lags, decays) {
  # Perform a grid search to find optimal lag and decay values for adstock transformation
  n_lags <- length(lags)
  n_decays <- length(decays)
  sse_values <- matrix(nrow = n_lags, ncol = n_decays)
  adstocked_values <- array(NA, dim = c(length(x), n_lags, n_decays))
  for (i in 1:n_lags) {
    for (j in 1:n_decays) {
      if (is.na(adstocked_values[1,i,j])) {
        adstocked_values[,i,j] <- adstock(x, lags[i], decays[j])
      }
      sse_values[i,j] <- sse(x, adstocked_values[,i,j])
    }
  }
  list(lags = lags, decays = decays, sse = sse_values)
}

## Example usage
# x <- c(10, 20, 30, 40, 50)
# lags <- seq(1, 3, by = 1)
# decays <- seq(0.1, 0.9, by = 0.1)
# results <- adstock_grid_search(x, lags, decays)

## Print the results of the grid search
# print(results)

## Find the optimal lag and decay values that minimize SSE
# min_sse <- min(results$sse)
# optimal_indices <- which(results$sse == min_sse, arr.ind = TRUE)
# optimal_lag <- results$lags[optimal_indices[1]]
# optimal_decay <- results$decays[optimal_indices[2]]
# cat("Optimal lag:", optimal_lag, "\n")
# cat("Optimal decay:", optimal_decay, "\n")
