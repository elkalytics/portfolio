#' Create categorical variables from numeric variables in a data frame
#'
#' This function takes a data frame and creates new categorical variables from every numeric variable in the data frame. For each numeric variable, new categorical variables are created by dividing the range of values into 2 through 10 equally-sized categories. The categories are represented as factor variables with names that include the name of the original variable and a group number (e.g., "x_Group1", "x_Group2", ..., "x_Group10").
#'
#' @param data a data frame containing numeric variables for which to create categorical variables
#'
#' @return a data frame with new categorical variables for each numeric variable in the input data frame
#'
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100), z = runif(100), w = rpois(100, 1))
#' data <- create_categories(data)
#' head(data)
create_categories <- function(data) {
  for (var in names(data)[sapply(data, is.numeric)]) {
    for (n in 2:10) {
      breaks <- quantile(jitter(data[[var]], factor = 1e-6), probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
      categories <- cut(data[[var]], breaks = breaks, labels = paste0(var, "_Group", 1:n))
      data[paste0(var, "_Group", n)] <- categories
    }
  }
  return(data)
}