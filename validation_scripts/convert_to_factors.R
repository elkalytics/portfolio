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
#' # Apply the is_categorical_df function
#' results <- is_categorical_df(df)
#' print(results)
#' @importFrom base sapply
#' @importFrom base length
#' @importFrom base names
#' @importFrom base unique
#' @importFrom base data.frame
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
#' Convert categorical variables in a data frame to factors
#'
#' This function takes a data frame as input and converts all categorical variables to factors.
#' A column is considered categorical if the ratio of unique values to total values is less than 0.95,
#' as determined by the is_categorical_df function.
#'
#' @param df A data frame
#' @return A new data frame with categorical variables converted to factors
#' @examples
#' df <- data.frame(
#'   col1 = c("A", "B", "C", "D", "E"),
#'   col2 = c(1, 2, 3, 4, 5),
#'   col3 = c("dog", "cat", "dog", "bird", "bird"),
#'   col4 = c(TRUE, FALSE, TRUE, FALSE, TRUE)
#' )
#' convert_to_factors(df)
#'
#' @importFrom base as.factor
#' @importFrom base subset
#' @importFrom base is.numeric
#' @importFrom base is.character
#' @importFrom base is.logical
#' @importFrom base is.factor
#' @export
convert_to_factors <- function(df) {
  categorical_vars <- is_categorical_df(df)
  categorical_vars <- categorical_vars[categorical_vars$categorical == TRUE,]
  
  df_factors <- df
  
  for (variable in categorical_vars$variable) {
    df_factors[[variable]] <- as.factor(df_factors[[variable]])
  }
  
  return(df_factors)
}