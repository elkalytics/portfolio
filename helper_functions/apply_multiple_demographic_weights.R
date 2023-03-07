#' Apply demographic weights to data
#'
#' Multiplies each numeric column of a data frame by the corresponding weight based on the levels of one or more specified categorical variables.
#'
#' @param data A data frame to be weighted.
#' @param demographic_vars A character vector of one or more categorical variables in \code{data} that will be used to determine the weights to apply.
#' @param demographic_weights A named list of named numeric vectors of weights to apply to each numeric column of \code{data} based on the levels of each categorical variable in \code{demographic_vars}.
#' @return A data frame with each numeric column weighted by the corresponding weight based on the levels of each categorical variable in \code{demographic_vars}.
#' @examples 
#' data <- data.frame(x = 1:5, y = 6:10, gender = c("male", "female", "male", "male", "female"), race = c("white", "black", "white", "black", "white"))
#' demographic_weights <- list(gender = c(male = 0.5, female = 1.5), race = c(white = 1.2, black = 0.8))
#' apply_multiple_demographic_weights(data, c("gender", "race"), demographic_weights)
#' @export
apply_multiple_demographic_weights <- function(data, demographic_vars, demographic_weights) {
  for (demographic_var in demographic_vars) {
    if (!is.factor(data[[demographic_var]])) {
      data[[demographic_var]] <- factor(data[[demographic_var]])
    }
  }
  if (!is.list(demographic_weights)) {
    stop("demographic_weights must be a list of named numeric vectors.")
  }
  for (demographic_var in demographic_vars) {
    if (!is.null(demographic_weights[[demographic_var]])) {
      if (!is.numeric(demographic_weights[[demographic_var]])) {
        stop(paste0("demographic_weights for '", demographic_var, "' must be a named numeric vector."))
      }
      if (any(!names(demographic_weights[[demographic_var]]) %in% levels(data[[demographic_var]]))) {
        stop(paste0("demographic_weights for '", demographic_var, "' must have a name for every level of '", demographic_var, "'."))
      }
    }
  }
  weighted_data <- data
  for (demographic_var in demographic_vars) {
    if (!is.null(demographic_weights[[demographic_var]])) {
      for (level in levels(data[[demographic_var]])) {
        level_weight <- demographic_weights[[demographic_var]][level]
        if (!is.numeric(level_weight)) {
          stop(paste0("demographic_weights for '", demographic_var, "' and level '", level, "' must be numeric."))
        }
        indices <- which(data[[demographic_var]] == level)
        weighted_data[indices, sapply(data, is.numeric)] <- data[indices, sapply(data, is.numeric)] * level_weight
      }
    }
  }
  return(weighted_data)
}