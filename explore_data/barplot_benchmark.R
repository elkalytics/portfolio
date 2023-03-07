#' Creates a barplot of means and standard errors for a numeric variable, grouped by a categorical variable
#'
#' @param data a data frame containing the variables to be plotted
#' @param x_var a string specifying the name of the categorical variable to be plotted on the x-axis
#' @param y_var a string specifying the name of the numeric variable to be plotted on the y-axis
#' @param color a logical value indicating whether to color-code the bars based on whether they are above or below the grand mean of \code{y_var} (default is \code{FALSE})
#'
#' @return a barplot of means and standard errors for each level of \code{x_var}, with or without color coding
#'
#' @examples
#' # create example dataset
#' set.seed(123) # for reproducibility
#' data <- data.frame(
#'   X = rep(letters[1:3], each = 4),
#'   Y = rnorm(12, mean = 10, sd = 2)
#' )
#' 
#' # create barplot without color coding
#' barplot_benchmark(data, "X", "Y")
#' 
#' # create barplot with color coding
#' barplot_benchmark(data, "X", "Y", color = TRUE)
#'
#' @importFrom ggplot2 aes geom_bar geom_errorbar geom_hline labs scale_fill_manual theme_minimal
#' @importFrom dplyr group_by mean n() sd summarize
#' @importFrom rlang sym
#' @importFrom stats is.numeric
#' @export
library(ggplot2)
library(dplyr)
library(rlang)
barplot_benchmark <- function(data, x_var, y_var, color = FALSE) {
  # check if Y is numeric
  if (!is.numeric(data[[y_var]])) {
    stop(paste(y_var, "is not numeric"))
  }
  
  # calculate the means and standard errors for each level of x_var
  means <- data %>% 
    group_by(!!sym(x_var)) %>% 
    summarize(mean = mean(!!sym(y_var)),
              se = sd(!!sym(y_var)) / sqrt(n()))
  
  # calculate the grand mean of y_var
  grand.mean <- mean(data[[y_var]])
  
  # create the barplot with error bars
  if (color) {
    # determine if each bar is above or below the grand mean
    above.line <- ifelse(means$mean >= grand.mean, TRUE, FALSE)
    
    # create a two-color palette for bars above and below the line
    colors <- c("yellow", "blue")[above.line + 1]
    
    ggplot(data = means, aes(x = !!sym(x_var), y = mean, fill = !!sym(x_var))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    width = 0.2, position = position_dodge(0.9)) +
      geom_hline(yintercept = grand.mean, linetype = "dashed") +
      scale_fill_manual(values = colors) +
      labs(title = paste("Barplot of Mean", y_var, "by", x_var),
           x = x_var, y = y_var) +
      theme_minimal()
  } else {
    ggplot(data = means, aes(x = !!sym(x_var), y = mean, fill = !!sym(x_var))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                    width = 0.2, position = position_dodge(0.9)) +
      geom_hline(yintercept = grand.mean, linetype = "dashed") +
      labs(title = paste("Barplot of Mean", y_var, "by", x_var),
           x = x_var, y = y_var) +
      theme_minimal()
  }
}