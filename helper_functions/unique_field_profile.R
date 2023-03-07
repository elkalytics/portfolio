#' Identify unique fields across multiple data frames
#'
#' This function takes a list of data frames as input and identifies the unique
#' fields (i.e., column names) across all the data frames. It then returns a
#' data frame with a binary matrix indicating which fields exist in each data
#' frame, along with a column indicating the name of each data frame.
#'
#' @param df_list A list of data frames.
#'
#' @return A data frame with a binary matrix indicating which fields exist in
#' each data frame, along with a column indicating the name of each data frame.
#'
#' @examples
#' # Create fake data
#' example_data_1 <- data.frame(A = 1:5, B = 6:10, C = 11:15)
#' example_data_2 <- data.frame(A = 11:15, B = 16:20, D = 21:25)
#' example_data_3 <- data.frame(C = 31:35, D = 36:40, E = 41:45)
#'
#' # Save data as a list
#' df_list <- list(example_data_1, example_data_2, example_data_3)
#'
#' # Use the function to save the results
#' unique_fields_df(df_list) -> results
#' View(results)
#'
#' @export
unique_fields_df <- function(df_list) {
  df_names <- names(df_list)
  field_names <- unique(unlist(lapply(df_list, names)))
  
  exists_matrix <- matrix(nrow = length(df_list), ncol = length(field_names))
  colnames(exists_matrix) <- field_names
  
  for (i in 1:length(df_list)) {
    for (j in 1:length(field_names)) {
      exists_matrix[i, j] <- ifelse(field_names[j] %in% names(df_list[[i]]), 1, 0)
    }
  }
  
  result_df <- as.data.frame(exists_matrix)
  result_df$df_name <- df_names
  
  return(result_df)
}