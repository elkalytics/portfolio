#' Impute missing values with column means for specified columns
#' 
#' This function replaces missing values in specified columns of a data frame with
#' the column means. By default, missing values in all continuous variables are 
#' replaced with the respective column means.
#' 
#' @param data A data frame with missing values
#' @param include_cols A numeric vector or character vector specifying the columns 
#'        to be imputed. If NULL, all continuous variables are imputed.
#' 
#' @return A data frame with missing values replaced with column means
#' 
#' @examples
#' 
#' # Create sample data
#' df <- data.frame(a = c(1, 2, NA, 4), 
#'                  b = c(NA, 3, 4, 5), 
#'                  c = c(2, 3, 4, 5), 
#'                  d = c(NA, NA, 6, 7))
#' 
#' # Impute missing data for all continuous variables
#' imputed_df <- impute_mean(df)
#' imputed_df
#' 
#' # Impute missing data for columns "a" and "c"
#' imputed_df_subset_name <- impute_mean(df, include_cols = c("a", "c"))
#' imputed_df_subset_name
#' 
#' # Impute missing data for columns "2" and "4"
#' imputed_df_subset_pos <- impute_mean(df, include_cols = c(2, 4))
#' imputed_df_subset_pos
#' 
impute_mean <- function(data, include_cols = NULL) {
  
  # If include_cols is not specified, assume all columns are to be included
  if (is.null(include_cols)) {
    include_cols <- seq_len(ncol(data))
  } else if (!is.numeric(include_cols)) {
    include_cols <- match(include_cols, colnames(data))
    include_cols <- include_cols[!is.na(include_cols)]
  }
  
  # Get the indices of continuous variables
  cont_vars <- sapply(data, is.numeric)
  
  # Subset data to only include specified columns and continuous variables
  data_subset <- data[, include_cols %in% seq_along(cont_vars) & cont_vars]
  
  # Replace missing values with column means
  means <- apply(data_subset, 2, mean, na.rm = TRUE)
  for (col in names(means)) {
    data_subset[is.na(data_subset[, col]), col] <- means[col]
  }
  
  # Replace original data with imputed data
  data[, include_cols %in% seq_along(cont_vars)] <- data_subset
  
  return(data)
}
