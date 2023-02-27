# Pivot entire table from wide to long

# Save function
pivot_to_long <- function(data, index_var, ...) {
  library(tidyr)
  pivot_longer(data, cols = c(...), 
               names_to = "variable", 
               values_to = "value", 
               names_pattern = "(.*)")
}


## Create sample data frame
# df <- data.frame(ID = c(1, 2, 3),
#                  A = c(10, 20, 30),
#                  B = c(15, 25, 35),
#                  C = c(18, 28, 38))

## Pivot data frame from wide to long
# new_df <- pivot_to_long(df, "ID", "A", "B", "C")

## View the result
# print(new_df)
