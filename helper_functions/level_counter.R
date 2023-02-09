# Count the number of levels/responses with a column

unique_levels_df <- function(df) {
  # Initialize an empty data frame to store the results
  res <- data.frame(variable = character(0),
                    unique_levels = numeric(0))
  
  # Loop through each column of the data frame
  for (col_name in colnames(df)) {
    # Count the number of unique levels in the column, excluding NA values
    levels <- length(unique(df[, col_name], na.rm = TRUE))
    # Add a row to the results data frame
    res <- rbind(res, data.frame(variable = col_name,
                                 unique_levels = levels))
  }
  
  # Return the results data frame
  return(res)
}


# Example usage
df <- data.frame(A = c(1, 2, 3, NA, 2),
                 B = c("a", "b", "a", "c", NA),
                 C = c(0, 1, 0, 1, 1),
                 D = c(3, 3, 3, 3, 3))

unique_levels_df(df) -> results
