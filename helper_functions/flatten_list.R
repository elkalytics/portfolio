# Function to merge multiple data frames
# Save data frame ID
# Merges values with same names into same column; assumes ID is unique per row

# Load packages
library(dplyr)

# Save function
merge_dataframes <- function(df_list, unique_id) {
  # Check that unique_id column exists in all data frames
  if (!all(unique_id %in% names(df_list[[1]]))) {
    stop("Unique ID column not found in all data frames")
  }
  # Combine data frames and stack variables with same name
  merged_data <- bind_rows(df_list, .id = "df_id") %>%
    group_by(!!sym(unique_id)) %>%
    summarise(across(everything(), ~ first(na.omit(.))))
  return(merged_data)
}


## Example data frames
# df1 <- data.frame(id = c(1, 2, 3), var1 = c("a", "b", "c"), var2 = c(4, 5, 6))
# df2 <- data.frame(id = c(4, 5, 6), var1 = c("d", "e", "f"), var3 = c(7, 8, 9))
# df3 <- data.frame(id = c(7, 8, 1), var4 = c(10, 11, 12), var5 = c(13, 14, 15))

## Save data frames as a list
# df_list <- list(df1, df2, df3)

## Use function
# merged_data <- merge_dataframes(df_list, "id")

## View result
# head(merged_data)