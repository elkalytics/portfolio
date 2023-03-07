#' Concatenate all fields of a data frame into one string with a delimiter
#'
#' @param df A data frame to concatenate
#' @return A character vector with concatenated fields
#'
#' @examples 
#' my_data <- data.frame(a = c(1, 2, 3), 
#'                       b = c("apple", "banana", "orange"), 
#'                       c = c(TRUE, FALSE, TRUE))
#'
#' concatenated <- concatenate_with_delimiter(my_data)
#' print(concatenated)
#'
#' @export 
concatenate_with_delimiter <- function(df) {
  concatenated <- apply(df, 1, paste, collapse = "!")
  return(concatenated)
}