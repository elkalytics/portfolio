#' Summarized Bar Plot Function
#'
#' This function creates a summarized bar plot using ggplot2 and dplyr.
#'
#' @param data a data frame
#' @param x a character string indicating the column name for the x-axis variable
#' @param y a character string indicating the column name for the y-axis variable
#' @return a ggplot object
#' @import ggplot2
#' @import dplyr
#' @examples
#' sum_barplot(diamonds, "cut", "price")
#'
#' @export
sum_barplot <- function(data, x, y) {
  library(ggplot2)
  library(dplyr)
  
  data_sum <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := sum(!!sym(y)))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "sum") +
    geom_text(data = data_sum, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}