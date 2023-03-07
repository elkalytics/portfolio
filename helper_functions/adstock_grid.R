#' Apply adstock transformation to a time series
#' 
#' This function applies an adstock transformation to a given time series using a specified lag and decay rate.
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
#' adstocked <- adstock(x, lag, decay)
library(dplyr)
library(doParallel)
library(foreach)
library(memoise)
library(future)
library(zoo)
library(furrr)
# Save adstock function
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
#' Calculate sum of squared errors between actual and predicted values
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
# Define a function to calculate the sum of squared errors between the predicted and actual values
sse <- function(actual, predicted) {
  # Calculate the sum of squared errors between the predicted and actual values
  sum((actual - predicted)^2)
}
#' Perform grid search for optimal lag and decay values for adstock transformation
#' 
#' This function performs a grid search to find the optimal lag and decay values for the adstock transformation of a given time series x. 
#' 
#' @param x a numeric vector representing the time series
#' @param lags a numeric vector of possible lag values to search over
#' @param decays a numeric vector of possible decay rate values to search over
#' 
#' @return a list containing the optimal lag and decay values and their corresponding sum of squared errors
#' 
#' @examples 
#' x <- c(10, 20, 30, 40, 50)
#' lags <- seq(1, 3, by = 1)
#' decays <- seq(0.1, 0.9, by = 0.1)
#' results <- adstock_grid_search(x, lags, decays)
#' @export
#'
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