relevel_lowprop <- function(data, threshold) {
  # Get the column names of factor variables
  factor_cols <- names(data)[sapply(data, is.factor)]
  
  # Loop through factor variables
  for (col in factor_cols) {
    # Get the proportions of each category
    prop_table <- prop.table(table(data[, col]))
    
    # Check if any category has a proportion less than the threshold
    if (any(prop_table < threshold)) {
      # Add "Other" to factor levels
      levels(data[, col]) <- c(levels(data[, col]), "Other")
      
      # Re-level the categories as "Other" in a new column
      new_col_name <- paste0(col, "_relevel")
      data[, new_col_name] <- data[, col]
      data[, new_col_name][data[, col] %in% names(prop_table[prop_table < threshold])] <- "Other"
      
      # Convert the new column to a factor variable with the new levels
      data[, new_col_name] <- factor(data[, new_col_name])
    }
  }
  
  # Return the modified data set
  return(data)
}



set.seed(123)  # For reproducibility

# Create a data frame with a numeric field and two categorical fields
example_data <- data.frame(
  numeric_field = rnorm(1000, mean = 10, sd = 2),
  categorical_field_1 = factor(sample(c("A", "B", "C", "D", "E"), 
                                      1000, replace = TRUE)),
  categorical_field_2 = factor(sample(c("On", "Off", "Uknown"), 
                                      1000, replace = TRUE, 
                                      prob = c(.95, .025, .025)))
)

# Print the first few rows of the data frame
relevel_lowprop(example_data, 0.05) -> results
