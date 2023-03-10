#' Save barplots for every numeric variable in data set by specified X variable
#' 
#' This function creates a barplot for every numeric variable in the data set, where
#' the mean of Y is calculated across each level of a specified X variable. The plots 
#' are saved with variable names 'outcome_by_X' in the working directory or a user 
#' specified directory using setwd(). 
#'
#' @param data A data frame.
#' @param x_var The name of the X variable to calculate means of Y across. Must be a 
#'   column name in \code{data}.
#' 
#' @examples
#' set.seed(123)
#' data <- data.frame(
#'   X = sample(letters[1:5], 100, replace = TRUE),
#'   Y = rnorm(100),
#'   Z = rpois(100, 5)
#' )
#' save_barplots(data, "X")
#' 
#' @export
save_barplots <- function(data, x_var) {
  # Select the specified X variable and all numeric variables in the data set
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  plot_vars <- c(x_var, numeric_vars)
  data_subset <- subset(data, select = plot_vars)
  
  # Loop over each numeric variable
  for (var in numeric_vars) {
    # Calculate the mean of Y for each level of the specified X variable
    means <- aggregate(data_subset[, var], by = list(data_subset[[x_var]]), FUN = mean)
    colnames(means) <- c(x_var, "Y_mean")
    
    # Add extra space above the highest bar
    max_val <- max(abs(means$Y_mean)) * 1.5
    
    # Create the barplot
    png(paste(var, "_by_", x_var, ".png", sep = ""), width = 800, height = 600)
    bp <- barplot(means$Y_mean, names.arg = means[[x_var]], xlab = x_var, ylab = paste0("Mean of ", var),
                  main = paste("Barplot of Mean of", var, "by", x_var), ylim = c(-max_val, max_val))
    
    # Add text labels for the mean value of Y within each bar
    for (i in seq_along(bp)) {
      if (means$Y_mean[i] >= 0) {
        text(x = bp[i], y = means$Y_mean[i] * 0.9, label = paste0("+", round(means$Y_mean[i], 2)), pos = 3)
      } else {
        text(x = bp[i], y = means$Y_mean[i] * 1.1, label = round(means$Y_mean[i], 2), pos = 3)
      }
    }
    
    # Save the barplot as a PNG in the working directory
    dev.off()
  }
}