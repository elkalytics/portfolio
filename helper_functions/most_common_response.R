# Most common responses for a single variable
library(dplyr)

most_common_responses <- function(data, var, top_n = 10) {
  result <- data %>%
    group_by(!!as.name(var)) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    head(top_n)
  result$percentage <- round(result$count / sum(result$count) * 100, 2)
  return(result)
}


################################################################# Use on one column in a table

data <- data.frame(
  id = 1:10,
  response = c("A", "B", "C", "A", "B", "A", "C", "B", "A", "C")
  )

most_common_responses(data, "response")


################################################################# Use on entire table

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


data <- data.frame(
  id = 1:10,
  response1 = c("A", "B", "C", "A", "B", "A", "C", "B", "A", "C"),
  response2 = c("Yes", "No", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"),
  response3 = c(1, 2, 2, 1, 2, 1, 2, 1, 2, 1),
  response4 = c(10, 20, 30, 40, 10, 20, 30, 40, 10, 20)
)


most_common_responses_all_vars(data) -> results
