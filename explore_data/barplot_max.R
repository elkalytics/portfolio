#' Max barplot
#'
#' This function creates a barplot showing the maximum value of a variable for each group of another variable.
#'
#' @param data A data frame.
#' @param x A character string or symbol indicating the column name for the x-axis.
#' @param y A character string or symbol indicating the column name for the y-axis.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' max_barplot(diamonds, "cut", "price")
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang sym !! 
#' @importFrom ggplot2 aes geom_bar geom_text position_dodge
max_barplot <- function(data, x, y) {
  library(ggplot2)
  library(dplyr)
  data_max <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := max(!!sym(y)))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "max") +
    geom_text(data = data_max, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}