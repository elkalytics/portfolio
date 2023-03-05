#' Count barplot function
#'
#' This function creates a barplot showing the count of each factor level in a given field of a data frame.
#'
#' @param data A data frame.
#' @param field A character string indicating the name of the field to plot.
#'
#' @return A ggplot object.
#'
#' @examples
#' count_barplot(diamonds, "cut")
#'
#' @import ggplot2
#'
#' @export
count_barplot <- function(data, field) {
  library(ggplot2)
  ggplot(data, aes(x = as.factor(.data[[field]]), fill = as.factor(.data[[field]]))) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -1)
}