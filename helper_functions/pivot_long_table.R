#' Pivot Table from Wide to Long
#'
#' This function takes a data frame, an index variable, and a set of columns to pivot
#' from wide to long format using the pivot_longer function from the tidyr package.
#'
#' @param data A data frame
#' @param index_var A string indicating the column name to use as the ID variable
#' @param ... A set of column names to pivot from wide to long format
#'
#' @return A data frame in long format
#'
#' @examples
#' df <- data.frame(ID = c(1, 2, 3),
#'                  A = c(10, 20, 30),
#'                  B = c(15, 25, 35),
#'                  C = c(18, 28, 38))
#' new_df <- pivot_to_long(df, "ID", "A", "B", "C")
#' print(new_df)
#'
#' @importFrom tidyr pivot_longer
#' @export
# Save function
pivot_to_long <- function(data, index_var, ...) {
  library(tidyr)
  pivot_longer(data, cols = c(...), 
               names_to = "variable", 
               values_to = "value", 
               names_pattern = "(.*)")
}