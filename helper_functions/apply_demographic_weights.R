#' Apply demographic weights to data
#'
#' Multiplies each numeric column of a data frame by the corresponding weight based on the levels of a specified categorical variable.
#'
#' @param data A data frame to be weighted.
#' @param demographic_var The name of a categorical variable in \code{data} that will be used to determine the weights to apply.
#' @param demographic_weights A named numeric vector of weights to apply to each numeric column of \code{data} based on the levels of \code{demographic_var}.
#' @return A data frame with each numeric column weighted by the corresponding weight based on the levels of \code{demographic_var}.
#' @examples 
#' data <- data.frame(x = 1:5, y = 6:10, gender = c("male", "female", "male", "male", "female"))
#' demographic_weights <- c(male = 0.5, female = 1.5)
#' apply_demographic_weights(data, "gender", demographic_weights)
#' @export
apply_demographic_weights <- function(data, demographic_var, demographic_weights) {
  if (!is.factor(data[[demographic_var]])) {
    data[[demographic_var]] <- factor(data[[demographic_var]])
  }
  if (!is.numeric(demographic_weights)) {
    stop("demographic_weights must be a named numeric vector.")
  }
  if (any(!names(demographic_weights) %in% levels(data[[demographic_var]]))) {
    stop("demographic_weights must have a name for every level of demographic_var.")
  }
  weighted_data <- data
  for (level in levels(data[[demographic_var]])) {
    level_weight <- demographic_weights[level]
    if (!is.numeric(level_weight)) {
      stop(paste0("demographic_weights for level '", level, "' must be numeric."))
    }
    indices <- which(data[[demographic_var]] == level)
    weighted_data[indices, sapply(data, is.numeric)] <- data[indices, sapply(data, is.numeric)] * level_weight
  }
  return(weighted_data)
}