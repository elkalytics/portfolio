#' Plot the difference between the data and a threshold value
#'
#' This function plots the difference between the data and a threshold value,
#' highlighting data points that are above or below the threshold with different
#' colors and labels.
#'
#' @param df a data frame containing the data to plot
#' @param threshold a numeric value representing the threshold to compare the data against
#' @param above_cat a character string representing the category for data above the threshold
#' @param below_cat a character string representing the category for data below the threshold
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @importFrom scales hue_pal
#' @importFrom stats median
#'
#' @examples
#' data <- data.frame(year = 1875:1972, level = LakeHuron)
#' plot_difference(data, threshold = 573.5, above_cat = "Above average", below_cat = "Below average")
#'
#' @export
plot_difference <- function(df, threshold = median(df$level), above_cat = "Above average", below_cat = "Below average") {
  
  # Load required packages outside of the function to avoid loading them multiple times
  requireNamespace("scales", quietly = TRUE)
  
  # Validate inputs using `stopifnot()` function to improve the code readability and avoid redundant checks
  stopifnot(
    is.data.frame(df),
    is.numeric(threshold),
    is.character(above_cat),
    is.character(below_cat)
  )
  
  ggplot(df, aes(x = year, y = level)) +
    stat_difference(aes(ymin = threshold, ymax = level), levels = c(above_cat, below_cat)) +
    geom_line() +
    geom_hline(yintercept = threshold) +
    scale_fill_manual(values = rev(hue_pal()(2)), labels = c(above_cat, below_cat), name = NULL) +
    ggtitle(paste("Threshold value:", threshold))
}