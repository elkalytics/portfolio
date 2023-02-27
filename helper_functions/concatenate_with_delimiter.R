# Function to concatenate all fields into one

# Save function
concatenate_with_delimiter <- function(df) {
  concatenated <- apply(df, 1, paste, collapse = "!")
  return(concatenated)
}

## Create example data
# my_data <- data.frame(a = c(1, 2, 3), 
#                       b = c("apple", "banana", "orange"), 
#                       c = c(TRUE, FALSE, TRUE))

## Use function
# concatenated <- concatenate_with_delimiter(my_data)

## View results
# print(concatenated)


