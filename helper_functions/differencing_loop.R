#' Determine Stationarity of Time Series
#'
#' This function determines the closest time series that is non-stationary by performing the best differencing step on the given time series.
#'
#' @param data The time series to be tested for stationarity.
#' @return A character string indicating which time series is closest to being non-stationary after performing the best differencing
#'
#' @examples
#' 
#' # Generate a time series with trend and seasonality
#' set.seed(123)
#' trend <- 0.05
#' seasonality <- 0.1
#' noise <- rnorm(100, mean = 0, sd = 1)
#' data <- cumsum(trend + seasonality * sin(2 * pi * 1:100 / 12) + noise)
#' 
#' # Test the example data - Original is the best
#' nonstationary_series <- suppressWarnings(determine_stationarity(data))
#' print(nonstationary_series)
#' 
#' # Generate a new time series with a unit root
#' set.seed(123)
#' data <- arima.sim(model = list(order = c(1, 1, 0), ar = 0.99), n = 1000)
#' 
#' # Add some noise to the time series
#' noise <- rnorm(1000, mean = 0, sd = 1)
#' data <- data + noise
#' 
#' # Test the determine_stationarity function on the example data - Differencing is better
#' nonstationary_series <- suppressWarnings(determine_stationarity(data))
#' print(nonstationary_series))
#'
#' # See p-value for each test
#' for (i in 1:10) {
#'   diff_data <- diff(data, differences = i)
#'   p_value <- adf.test(diff_data)$p.value
#'   cat(paste0("Diff", i, " p-value: ", p_value, "\n"))
#' }
#'
#' @import tseries
#' @export
library(tseries) # load the tseries package for stationarity tests

determine_stationarity <- function(data) {
  
  # loop through each difference step (up to 10) and test for stationarity on the resulting time series
  stationarity_values <- numeric(10)
  for (i in 1:10) {
    diff_data <- diff(data, differences = i)
    stationarity_values[i] <- adf.test(diff_data)$p.value
  }
  
  # determine which time series is closest to being non-stationary after performing the best differencing step
  nonstationary_series <- ""
  best_diff <- which.min(stationarity_values)
  if (stationarity_values[1] <= 0.05) {
    nonstationary_series <- "Original"
  } else if (stationarity_values[best_diff] <= 0.05) {
    nonstationary_series <- paste0("Diff", best_diff)
  } else {
    nonstationary_series <- "None"
  }
  
  # return the name of the time series that is closest to being non-stationary after performing the best differencing step
  return(nonstationary_series)
}