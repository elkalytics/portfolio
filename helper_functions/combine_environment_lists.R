# Load library
library(dplyr)
# Load data
box_office_all <- readRDS("box_office_all.RDS")
#' Convert nested data frame to character data frame
#'
#' This function takes a nested data frame and converts each nested data frame into a character data frame.
#' 
#' @param nested_df a nested data frame
#' @param date the name of the date column to add to the output data frame
#'
#' @return a character data frame with the same column names as the input data frame
#'
#' @examples
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' nested_df <- list(df, df)
#' names(nested_df) <- c("2020-01-01", "2020-01-02")
#' nested_to_char(nested_df, "date")
#'
#' @export
# Define a function to convert each nested data frame into a character data frame
nested_to_char <- function(nested_df, date) {
  as.data.frame(lapply(nested_df, as.character), stringsAsFactors = FALSE) %>%
    mutate(date = rep(date, nrow(.)))
}
#' Bind multiple nested data frames into a single data frame with character strings
#'
#' @param box_office_all A list of nested data frames
#' @return A single data frame with character strings
#'
#' @importFrom dplyr bind_rows
#'
#' @examples
#' box_office_all <- list(
#' list(data.frame(A = c("a", "b", "c"), B = c(1, 2, 3)),
#' data.frame(A = c("d", "e", "f"), B = c(4, 5, 6))),
#' list(data.frame(A = c("g", "h", "i"), B = c(7, 8, 9)),
#' data.frame(A = c("j", "k", "l"), B = c(10, 11, 12)))
#' )
#' df_list <- bind_nested_data_frames(box_office_all)
#'
#' @export
# Apply the function to each element of the nested list and bind the resulting data frames
df_list <- lapply(box_office_all, function(nested_list) {
  lapply(nested_list, function(nested_df) {
    nested_to_char(nested_df, names(nested_list)[1])
  }) %>%
    bind_rows()
}) %>%
  bind_rows()