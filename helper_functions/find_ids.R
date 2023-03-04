# Function to quickly find columns to join on

# Save function
find_ids <- function(df_list) {
  library(dplyr)
  
  # Create a data frame to store the results
  result_df <- data.frame(Data_Frame = integer(),
                          Variable = character(),
                          Common = character(),
                          stringsAsFactors = FALSE)
  
  # Compare each pair of data frames
  for (i in 1:length(df_list)) {
    for (j in (i+1):length(df_list)) {
      
      # Check if index j is within bounds of df_list
      if(j <= length(df_list)) {
        df1 <- df_list[[i]]
        df2 <- df_list[[j]]
        
        # Compare each column in the two data frames
        for (col in colnames(df1)) {
          if (col %in% colnames(df2)) {
            # If the column is present in both data frames, add it to the result data frame
            result_df <- bind_rows(result_df, data.frame(Data_Frame = c(i, j),
                                                         Variable = col,
                                                         Common = "Yes",
                                                         stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
  
  # Convert "Data_Frame" column to integer type
  result_df$Data_Frame <- as.integer(result_df$Data_Frame)
  
  # Return the result data frame
  return(result_df)
}

## Create some example data frames
# df1 <- data.frame(ID = 1:5, Name = c("John", "Mary", "Bob", "Alice", "Tom"))
# df2 <- data.frame(ID = 3:7, Age = c(25, 32, 46, 18, 57))
# df3 <- data.frame(ID = 2:6, Gender = c("M", "F", "M", "F", "M"))

## Create a list of the data frames
# df_list <- list(df1, df2, df3)

## Call the compare_columns function
# find_ids(df_list) -> results

## Review results 
# head(results)

