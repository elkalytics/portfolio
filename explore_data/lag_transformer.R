#' Apply multiple lag transformations
#'
#' This function applies multiple lag transformations, differencing, and logarithmic transformations to a given dataset.
#' @param data a numeric vector or matrix containing the data to be transformed
#' @param lags an integer specifying the number of lags to be included in the transformed dataset
#' @param diff_lags an integer specifying the number of times to difference the data
#' @param log_transform a logical indicating whether to perform a logarithmic transformation on the data
#' @return a matrix containing the transformed data
#' @examples 
#' # Load data
#' data(AirPassengers)
#' # Apply function
#' transformed_data <- multi_lag_transform(AirPassengers, lags = 2, diff_lags = 2, log_transform = TRUE)
multi_lag_transform <- function(data, lags, diff_lags, log_transform) {
  
  # Check for zero or negative values
  if (any(data <= 0)) {
    stop("Data contains zero or negative values and cannot be log transformed")
  }
  
  # Add constant if necessary
  if (mean(data) != 0) {
    data <- data - mean(data)
  }
  
  # Perform differencing if necessary
  if (diff_lags > 0) {
    diff_data <- diff(data, diff_lags)
    diff_data <- na.omit(diff_data)
    message("Data dimensions after difference transformation: ", dim(diff_data))
  } else {
    diff_data <- data
  }
  
  # Perform logarithmic transformation if necessary
  if (log_transform) {
    diff_data <- log(diff_data)
    message("Data dimensions after log transformation: ", dim(diff_data))
  }
  
  # Create lagged data matrix
  lagged_data <- matrix(0, nrow = length(diff_data) - lags, ncol = lags)
  for (i in 1:lags) {
    lagged_data[, i] <- diff_data[(lags - i + 1):(length(diff_data) - i)]
  }
  
  # Combine original and lagged data
  transformed_data <- cbind(diff_data[-(1:lags)], lagged_data)
  
  message("Transformed data dimensions: ", dim(transformed_data))
  return(transformed_data)
}