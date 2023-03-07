#' Adstock transformation of a time series
#' 
#' This function applies an adstock transformation to a time series using a specified lag and decay.
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
# Save adstock function
adstock <- function(x, lag, decay) {
  weights <- c(1, rep(decay, lag))
  convolved <- numeric(length(x))
  for (i in (lag+1):length(x)) {
    convolved[i] <- sum(weights * x[(i-lag):i])
  }
  for (i in 1:lag) {
    convolved[i] <- sum(weights[1:(i+1)] * x[1:(i+1)])
  }
  return(convolved)
}