#' Apply weights to data
#'
#' Multiplies each column of a data frame by the corresponding weight in a numeric vector.
#'
#' @param data A data frame to be weighted.
#' @param weights A numeric vector of weights to apply to each column of \code{data}.
#' @return A data frame with each column weighted by the corresponding weight in \code{weights}.
#' @examples 
#' data <- data.frame(x = 1:5, y = 6:10)
#' weights <- c(0.5, 1, 2, 1.5, 0.8)
#' apply_weights(data, weights)
#' @export
apply_weights <- function(data, weights) {
  weighted_data <- data * weights
  return(weighted_data)
}