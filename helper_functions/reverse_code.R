#' Recode Numeric Columns
#'
#' Recode numeric columns in a data frame by sorting unique values and
#' assigning them new codes in descending order.
#'
#' @param df A data frame.
#' @param cols A character vector of column names or numeric indices to be recoded.
#' @return A data frame with recoded columns.
#' @examples
#' df <- data.frame(
#'   x = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
#'   y = c(0, 0, 1, 1, 0, 1, 1, 0, 0, 1),
#'   z = c(4, 4, 1, 1, 3, NA, 2, 3, 3, 1)
#' )
#'
#' # Apply function to columns
#' reverse_code(df, "x")
#' reverse_code(df, "y")
#' reverse_code(df, "z")
#' reverse_code(df, c("x", "z"))
#' @export
# Save function
reverse_code <- function(df, cols) {
  
  # Convert cols to numeric indices if necessary
  if (is.character(cols)) {
    cols <- match(cols, names(df))
  }
  
  # Check if columns contain only whole numbers
  if (!all(sapply(df[cols], function(x) all(is.na(x) | x == as.integer(x))))) {
    stop("Columns must contain whole numbers only.")
  }
  
  # Determine recoding scheme for each column
  for (col in cols) {
    # Omit missing values
    x <- df[[col]][!is.na(df[[col]])]
    
    # Determine number of unique values
    unique_vals <- sort(unique(x))
    n_vals <- length(unique_vals)
    
    # Determine recoding scheme
    recode_vals <- unique_vals[n_vals:1]
    
    # Recode column using recoding scheme
    df[[col]][!is.na(df[[col]])] <- recode_vals[match(x, unique_vals)]
  }
  
  return(df)
}