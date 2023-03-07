#' Most Common Responses
#'
#' This function takes a data frame and a variable name, and returns a table
#' with the most common responses for that variable, their respective counts,
#' and percentages. By default, it returns the top 10 most common responses.
#'
#' @param data A data frame
#' @param var A string indicating the column name for which to find the most common responses
#' @param top_n An integer indicating the number of top responses to return
#'
#' @return A data frame with the most common responses for the variable, their counts,
#' and percentages
#'
#' @examples
#' data <- data.frame(id = 1:10, response = c("A", "B", "C", "A", "B", "A", "C", "B", "A", "C"))
#' most_common_responses(data, "response")
#'
#' @importFrom dplyr group_by summarise arrange head
#' @export
# Load library
library(dplyr)
# Save function
most_common_responses <- function(data, var, top_n = 10) {
  result <- data %>%
    group_by(!!as.name(var)) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    head(top_n)
  result$percentage <- round(result$count / sum(result$count) * 100, 2)
  return(result)
}
#' Most Common Responses for All Variables
#'
#' This function takes a data frame and returns a list of tables with the most
#' common responses for all character and factor variables in the data frame.
#' By default, it returns the top 10 most common responses for each variable.
#'
#' @param data A data frame
#' @param top_n An integer indicating the number of top responses to return
#'
#' @return A list of data frames with the most common responses for each character
#' and factor variable in the data frame, their counts, and percentages
#'
#' @examples
#' data <- data.frame(id = 1:10,
#'                    response1 = c("A", "B", "C", "A", "B", "A", "C", "B", "A", "C"),
#'                    response2 = c("Yes", "No", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"),
#'                    response3 = c(1, 2, 2, 1, 2, 1, 2, 1, 2, 1),
#'                    response4 = c(10, 20, 30, 40, 10, 20, 30, 40, 10, 20))
#' most_common_responses_all_vars(data)
#'
#' @importFrom dplyr group_by summarise arrange head
#' @export
most_common_responses_all_vars <- function(data, top_n = 10) {
  result_list <- lapply(names(data), function(var) {
    if (is.factor(data[[var]]) || is.character(data[[var]])) {
      most_common_responses(data, var, top_n)
    } else {
      NULL
    }
  })
  processed_vars <- which(sapply(result_list, function(x) !is.null(x)))
  result_list <- result_list[processed_vars]
  names(result_list) <- names(data)[processed_vars]
  return(result_list)
}