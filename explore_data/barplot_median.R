#' Create a barplot with the median of a variable by a categorical variable
#'
#' This function takes a data frame, a variable to group by (categorical), and a variable to compute the median for.
#' It creates a barplot where each bar represents the median value of the variable for each group.
#'
#' @param data A data frame containing the variables.
#' @param x A character string with the name of the categorical variable to group by.
#' @param y A character string with the name of the variable to compute the median for.
#'
#' @return A ggplot object with a barplot.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' median_barplot(diamonds, "cut", "price")
#' }
#'
#' @export
median_barplot <- function(data, x, y) {
  library(ggplot2)
  library(dplyr)
  data_median <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := median(!!sym(y)))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "median") +
    geom_text(data = data_median, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}