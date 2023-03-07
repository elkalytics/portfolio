#' Function to take earliest non-null row
#'
#' This function takes a data frame and finds the earliest non-null value for each column, 
#' grouping by a unique identifier. The output is a data frame with one row for each 
#' unique identifier and columns representing the earliest non-null value for each 
#' column in the group.
#'
#' @param data A data frame containing the data to filter
#' @param system_date_col A string specifying the name of the column containing the system date
#' @param unique_id_col A string specifying the name of the column containing the unique identifier
#' 
#' @return A data frame with one row for each unique identifier and columns representing the earliest 
#' non-null value for each column in the group.
#'
#' @import dplyr
#'
#' @examples
#' data <- data.frame(
#'   unique_id = c("A", "A", "B", "B", "C", "C"),
#'   system_date = c("2022-01-01", "2022-01-02", "2022-01-01", "2022-01-03", "2022-01-02", "2022-01-03"),
#'   col1 = c(NA, 2, 3, 4, 5, 6),
#'   col2 = c(7, NA, 9, 10, 11, 12),
#'   col3 = c(13, 14, NA, 16, 17, 18)
#' )
#'
#' earliest_non_null_values(data, "system_date", "unique_id")
#'
#' @export
# Load package
library(dplyr)
# Save function
earliest_non_null_values <- function(data, system_date_col, unique_id_col) {
  # Get the names of the columns to filter (all columns except system_date and unique_id)
  cols_to_filter <- setdiff(names(data), c(system_date_col, unique_id_col))
  
  # Group the data by unique_id
  grouped_data <- split(data, data[[unique_id_col]])
  
  # Create an empty data frame to hold the filtered data
  filtered_data <- data.frame(stringsAsFactors = FALSE)
  
  # Loop over each group of data
  for (i in seq_along(grouped_data)) {
    # Get the current group
    group <- grouped_data[[i]]
    
    # Find the earliest non-null value for each column in the group
    values_to_add <- list()
    values_to_add[[unique_id_col]] <- group[[unique_id_col]][1]
    values_to_add[[system_date_col]] <- head(group[[system_date_col]], 1)
    for (col in cols_to_filter) {
      first_non_null <- head(which(!is.na(group[[col]])), 1)
      if (!is.na(first_non_null)) {
        values_to_add[[col]] <- group[[col]][first_non_null]
      }
    }
    
    # Add the filtered row to the filtered data frame
    filtered_data <- rbind(filtered_data, values_to_add)
  }
  
  # Remove duplicate rows from the filtered data
  filtered_data <- unique(filtered_data)
  
  # Return the filtered data
  return(filtered_data)
}