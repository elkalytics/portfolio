#' Vectorized adstock transformation of a time series
#' 
#' This function applies an adstock transformation to a time series using vectorized operations.
#' 
#' @param x a numeric vector representing the time series
#' @param lag an integer representing the lag for the adstock transformation
#' @param decay a numeric value representing the decay rate for the adstock transformation
#' 
#' @return a numeric vector representing the adstock transformed time series
#' 
#' @examples 
#' x <- c(10, 20, 30, 40, 50)
#' lag <- 2
#' decay <- 0.5
#' adstocked <- adstock_vectorized(x, lag, decay)
#' @export
#' 
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
#' Calculate the sum of squared errors between actual and predicted values
#' 
#' This function calculates the sum of squared errors between the actual and predicted values of a given time series.
#' 
#' @param actual a numeric vector representing the actual values of a time series
#' @param predicted a numeric vector representing the predicted values of a time series
#' 
#' @return a numeric value representing the sum of squared errors
#' 
#' @examples 
#' actual <- c(10, 20, 30, 40, 50)
#' predicted <- c(11, 19, 31, 39, 51)
#' sse_value <- sse(actual, predicted)
#' @export
#' 
sse <- function(actual, predicted) {
  # Calculate the sum of squared errors between the predicted and actual values
  sum((actual - predicted)^2)
}
#' Perform grid search for optimal lag and decay values for adstock transformation using parallel processing
#' 
#' This function performs a grid search to find the optimal lag and decay values for the adstock transformation of a given time series x. 
#' 
#' @param x a numeric vector representing the time series
#' @param lags a numeric vector of possible lag values to search over
#' @param decays a numeric vector of possible decay rate values to search over
#' @param cores an integer representing the number of CPU cores to use for parallel processing
#' 
#' @return a list containing the optimal lag and decay values and their corresponding sum of squared errors
#' 
#' @examples 
#' set.seed(123)
#' x <- rnorm(100, mean = 10, sd = 3)
#' lags <- seq(1, 10, by = 1)
#' decays <- seq(0.1, 0.9, by = 0.1)
#' results <- adstock_grid_search(x, lags, decays, cores = 2)
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