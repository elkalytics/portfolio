#' Check if all non-NA values in specified columns are the same for each row
#'
#' Given a data frame, this function checks if all non-NA values in the specified columns are the same for each row.
#' A new column is added to the data frame indicating whether the values are the same or not.
#' 
#' @param data A data frame.
#' @param start_col The starting column of the subset of columns to check. Can be either the name of the column or the position of the column.
#' @param end_col The ending column of the subset of columns to check. Can be either the name of the column or the position of the column.
#' 
#' @return A modified version of the input data frame with a new column added indicating whether the values in the specified columns are the same or not.
#'
#' @examples 
#' my_data <- data.frame(var1= c(1, 2, 3), 
#'                       var2 = c(NA, 4, 4), 
#'                       var3 = c(NA, 4, 6), 
#'                       var4 = c(7, 4, 7), 
#'                       var5 = c("x", "y", "z"),
#'                       var6 = c("x", "x", "x"))
#' 
#' example1 <- response_patterns(my_data, "var1", "var2")
#' head(example1)
#' 
#' example2 <- response_patterns(my_data, 2, 4)
#' head(example2)
#' 
#' example3 <- response_patterns(my_data, 5, 6)
#' head(example3)
#' 
#' example4 <- response_patterns(my_data, "var2", "var3")
#' head(example4)
#'
#' @export
response_patterns <- function(data, start_col, end_col) {
  # Convert column names to positions if necessary
  if (!is.numeric(start_col)) {
    start_col <- which(names(data) == start_col)
  }
  if (!is.numeric(end_col)) {
    end_col <- which(names(data) == end_col)
  }
  
  # Extract the columns specified by start_col and end_col
  cols <- data[, start_col:end_col]
  
  # Determine if all non-NA values are the same for each row
  same_values <- apply(cols, 1, function(x) length(unique(na.omit(x))) == 1)
  
  # Create a new column with 1 if all values are the same, 0 if not
  col_name <- paste0(names(data)[start_col], "_", names(data)[end_col])
  data[[col_name]] <- as.numeric(same_values)
  
  # Return the modified data frame
  return(data)
}