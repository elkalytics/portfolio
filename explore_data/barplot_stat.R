#' Barplot with various statistics
#' 
#' This function makes a barplot with labels for the statistic the user specifies.
#' It is a combination of the sum, mean, median, min, and max barplot functions.
#' It generates a count of the variable specified if no statistic is stated,
#' but will generate the desired statistic by level of the other variable to user calls.
#' For example, in the diamonds dataset, 'barplot_stat(diamonds, "cut") will generate
#' counts by cut in the data. But specifying 'barplot_stat(diamonds, "cut", "price", mean)
#' will provide a barplot of the mean price for each cut.
#' 
#' @param data A data.frame or tibble
#' @param x A string with the name of the column that should be plotted on the x-axis
#' @param y A string with the name of the column that should be plotted on the y-axis
#' @param stat_type A string indicating which statistic to use for y. Options are "sum", "mean", "median", "min", and "max"
#'
#' @return A ggplot object with a barplot and optional text labels
#' 
#' @examples
#' barplot_stat(diamonds, "cut")
#' barplot_stat(diamonds, "cut", "price", "mean")
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang sym
#' 
#' @export
barplot_stat <- function(data, x, y = NULL, stat_type = NULL) {
  library(ggplot2)
  library(dplyr)
  
  stat_functions <- c("sum", "mean", "median", "min", "max")
  
  if (!is.null(stat_type) && !stat_type %in% stat_functions) {
    stop("Invalid stat_type. Choose one of the following: sum, mean, median, min, max.")
  }
  
  if (is.null(y) && is.null(stat_type)) {
    ggplot(data, aes(x = as.factor(.data[[x]]), fill = as.factor(.data[[x]]))) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -1)
  } else {
    data_stat <- data %>% 
      group_by(!!sym(x)) %>% 
      summarize(y := ifelse(is.null(stat_type), length(!!sym(y)), get(ifelse(is.character(stat_type), stat_type, "sum"))(!!sym(y))))
    
    if (!is.null(stat_type) && is.numeric(data_stat$y)) {
      data_stat$y <- round(data_stat$y, 2)
    }
    
    ggplot(data_stat) + 
      geom_bar(aes(!!sym(x), y, fill = as.factor(!!sym(x))), 
               position = "dodge", stat = "summary", fun = ifelse(is.null(stat_type), "count", ifelse(is.character(stat_type), stat_type, "sum"))) +
      geom_text(data = data_stat, aes(label = y, x = !!sym(x), y = y), 
                position = position_dodge(width = 0.9), vjust = -0.5)
  }
}