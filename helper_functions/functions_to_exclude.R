# Function to select columns with sufficient finite observations
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
