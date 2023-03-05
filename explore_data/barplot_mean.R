#' Create a mean barplot
#' 
#' This function creates a mean barplot by grouping the data by a specified
#' variable, calculating the mean of another specified variable for each group,
#' and then plotting the means using ggplot2.
#' 
#' @param data A data frame.
#' @param x A string indicating the variable to group by.
#' @param y A string indicating the variable to calculate the mean of and plot.
#' 
#' @return A ggplot object.
#' 
#' @import ggplot2
#' @import dplyr
#' 
#' @examples
#' \dontrun{
#' mean_barplot(diamonds, "cut", "price")
#' }
#'
#' @export
mean_barplot <- function(data, x, y) {
  library(ggplot2)
  library(dplyr)
  
  data_mean <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := round(mean(!!sym(y)), 2))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "mean") +
    geom_text(data = data_mean, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}