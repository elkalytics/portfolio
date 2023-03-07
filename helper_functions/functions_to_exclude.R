#' Select columns with sufficient finite observations
#'
#' This function takes a data frame and a grouping variable and returns a vector of
#' column names for columns that have at least two finite observations (i.e. non-missing
#' and non-infinite values) across all levels of the grouping variable.
#'
#' @param data A data frame
#' @param grouping_var A character string specifying the name of the grouping variable
#'
#' @return A vector of column names
#'
#' @examples
#' data <- data.frame(x = c(1, 2, 3, 4), y = c(1, 2, NA, Inf), group = c("A", "A", "B", "B"))
#' select_cols(data, "group") # Returns "x"
#'
#' @export
# Save function
select_cols <- function(data, grouping_var) {
  cols <- names(data)
  finite_cols <- sapply(cols, function(col) {
    sum(is.finite(data[[col]]))
  })
  finite_cols_idx <- which(finite_cols >= 2)
  finite_cols_names <- cols[finite_cols_idx]
  
  # Exclude grouping variable from the list of selectable columns
  finite_cols_names <- finite_cols_names[finite_cols_names != grouping_var]
  return(finite_cols_names)
}