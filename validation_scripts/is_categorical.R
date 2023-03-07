#' Check if each column in a data frame is categorical
#'
#' This function takes a data frame as input and returns a data frame indicating
#' which columns are categorical. A column is considered categorical if the ratio of
#' unique values to total values is less than 0.95.
#'
#' @param df A data frame
#' @return A data frame with two columns: "variable" (the column names) and "categorical" (logical value indicating if column is categorical or not)
#' @examples
#' df <- data.frame(
#'   col1 = c("A", "B", "C", "D", "E"),
#'   col2 = c(1, 2, 3, 4, 5),
#'   col3 = c("dog", "cat", "dog", "bird", "bird"),
#'   col4 = c(TRUE, FALSE, TRUE, FALSE, TRUE)
#' )
#' is_categorical_df(df)
#'
#' @export
# Save function
is_categorical_df <- function(df) {
  results <- sapply(df, function(col) {
    unique_count <- length(unique(col))
    total_count <- length(col)
    ratio <- unique_count / total_count
    
    if (ratio < 0.95) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  results_df <- data.frame(variable = names(results),
                           categorical = results)
  return(results_df)
}