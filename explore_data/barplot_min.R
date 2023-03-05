#' Create a barplot showing the minimum value of a variable for each group
#'
#' This function takes a data frame and two variable names and creates a barplot
#' that shows the minimum value of the second variable for each group defined by the
#' first variable.
#'
#' @param data A data frame.
#' @param x The name of the variable to use for grouping.
#' @param y The name of the variable to calculate the minimum of.
#'
#' @return A ggplot object.
#'
#' @examples
#' min_barplot(diamonds, "cut", "price")
#'
#' @import ggplot2
#' @import dplyr
min_barplot <- function(data, x, y) {
  library(ggplot2)
  library(dplyr)
  
  data_min <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := min(!!sym(y)))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "min") +
    geom_text(data = data_min, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}