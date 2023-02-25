# Combined function for adstock grid
# Vectorized and parallel processing for larger data

# Load packages
library(parallel)
library(memoise)
library(doParallel)

# Save function
adstock_vectorized <- function(x, lag, decay) {
  # Apply adstock transformation to a vector x using vectorized operations
  weights <- seq(1, lag) * decay
  weights <- weights / sum(weights)
  rollapply(x, width = lag, FUN = function(x) sum(x * weights), align = "right", fill = NA)
}

sse <- function(actual, predicted) {
  # Calculate the sum of squared errors between the predicted and actual values
  sum((actual - predicted)^2)
}

adstock_grid_search <- function(x, lags, decays, cores = 1) {
  # Perform a grid search to find optimal lag and decay values for adstock transformation
  adstocked_values <- array(NA_real_, dim = c(length(x), length(lags), length(decays)))
  
  # Define a memoized version of the adstock function to reuse computed values
  adstock_memo <- memoise(function(x, lag, decay) {
    if (is.na(adstocked_values[1, which(lags == lag), which(decays == decay)])) {
      adstocked_values[, which(lags == lag), which(decays == decay)] <<- adstock_vectorized(x, lag, decay)
    }
    adstocked_values[, which(lags == lag), which(decays == decay)]
  })
  
  # Perform grid search using parallel processing
  cl <- makeCluster(cores)
  clusterEvalQ(cl, {
    library(zoo)
    library(memoise)
    adstock_vectorized <- function(x, lag, decay) {
      # Apply adstock transformation to a vector x using vectorized operations
      weights <- seq(1, lag) * decay
      weights <- weights / sum(weights)
      rollapply(x, width = lag, FUN = function(x) sum(x * weights), align = "right", fill = NA)
    }
    sse <- function(actual, predicted) {
      # Calculate the sum of squared errors between the predicted and actual values
      sum((actual - predicted)^2)
    }
  })
  registerDoParallel(cl)
  sse_values <- foreach(lag = lags, .combine = "cbind") %:%
    foreach(decay = decays, .combine = "c") %dopar% {
      predicted <- adstock_memo(x, lag, decay)
      sse(x, predicted)
    }
  stopCluster(cl)
  sse_values <- matrix(sse_values, nrow = length(lags), ncol = length(decays))
  list(lags = lags, decays = decays, sse = sse_values)
}

## Make fake data
# set.seed(123)
# x <- rnorm(100, mean = 10, sd = 3)
# lags <- seq(1, 10, by = 1)
# decays <- seq(0.1, 0.9, by = 0.1)

## Save results
# results <- adstock_grid_search(x, lags, decays, cores = 2)
