# Load library
library(dplyr)

# Load data
box_office_all <- readRDS("box_office_all.RDS")

# Define a function to convert each nested data frame into a character data frame
nested_to_char <- function(nested_df, date) {
  as.data.frame(lapply(nested_df, as.character), stringsAsFactors = FALSE) %>%
    mutate(date = rep(date, nrow(.)))
}

# Apply the function to each element of the nested list and bind the resulting data frames
df_list <- lapply(box_office_all, function(nested_list) {
  lapply(nested_list, function(nested_df) {
    nested_to_char(nested_df, names(nested_list)[1])
  }) %>%
    bind_rows()
}) %>%
  bind_rows()
