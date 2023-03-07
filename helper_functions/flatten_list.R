#' Merge multiple data frames based on a unique ID column
#'
#' This function merges multiple data frames into a single data frame based on a unique
#' ID column. Values with the same name are merged into the same column. The function
#' assumes that the ID column is unique per row.
#'
#' @param df_list A list of data frames to merge
#' @param unique_id The name of the unique ID column
#'
#' @return A merged data frame
#'
#' @importFrom dplyr bind_rows group_by summarise across everything first coalesce
#' @export
#'
#' @examples
#' df1 <- data.frame(id = c(1, 2, 3), var1 = c("a", "b", "c"), var2 = c(4, 5, 6))
#' df2 <- data.frame(id = c(4, 5, 6), var1 = c("d", "e", "f"), var3 = c(7, 8, 9))
#' df3 <- data.frame(id = c(7, 8, 1), var4 = c(10, 11, 12), var5 = c(13, 14, 15))
#' df_list <- list(df1, df2, df3)
#' merged_data <- merge_dataframes(df_list, "id") # Returns a merged data frame
# Load packages
library(dplyr)
# Save function
merge_dataframes <- function(df_list, unique_id) {
  stopifnot(all(unique_id %in% names(df_list[[1]])))
  bind_rows(df_list, .id = "df_id") %>%
    group_by(.data[[unique_id]]) %>%
    summarise(across(everything(), ~first(coalesce(., NA))))
}