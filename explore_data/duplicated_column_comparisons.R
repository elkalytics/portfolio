#' Count Columns Across Tables
#'
#' This function takes a list of data.frames as input and returns a data.frame 
#' containing information on the columns across all tables in the list.
#'
#' @param tables A list of data.frames.
#'
#' @return A data.frame containing information on the columns across all tables 
#' in the list. For each column, the output includes the column name, the name(s) 
#' of the table(s) in which the column appears, and the first non-null value in 
#' that column.
#'
#' @import janitor
#'
#' @examples
#' df1 <- data.frame(col1 = c(1, 2, 3), col2 = c(4, 5, 6), col3 = c(7, 8, 9))
#' df2 <- data.frame(col1 = c(5, 10), col2 = c(3, 4), col3 = c(5, 6), col4 = c(7, 8))
#' df3 <- data.frame(col1 = c(5, 10), col4 = c("A", "B"))
#' tables <- list(df1 = df1, df2 = df2, df3 = df3)
#' count_cols_across_tables(tables)
#'
#' @export
count_cols_across_tables <- function(tables) {
  library(janitor)
  
  cleaned_tables <- lapply(tables, function(x) clean_names(x))
  col_counts <- lapply(cleaned_tables, function(x) colSums(!is.na(x)))
  col_data <- lapply(cleaned_tables, function(x) {
    lapply(colnames(x), function(col) {
      non_null_val <- x[!is.na(x[[col]]), col][1]
      list(col = col, first_non_null = non_null_val)
    })
  })
  
  col_data <- unlist(col_data, recursive = FALSE)
  col_data <- lapply(col_data, function(x) {
    x$tables <- names(cleaned_tables)[sapply(col_counts, function(y) y[x$col] > 0)]
    x
  })
  
  col_data <- do.call(rbind, col_data)
  col_data <- as.data.frame(col_data)
  
  col_data
}