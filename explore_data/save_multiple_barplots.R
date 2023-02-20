# Function to save barplot for every numeric variable in table
# User specifies data and X variable they want the mean of Y across
# Plots will save with variable names 'outcome_by_X'
# Use getwd() to determine where they are saved or setwd() to change directory

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

## Example data set
# set.seed(123)
# data <- data.frame(
#   X = sample(letters[1:5], 100, replace = TRUE),
#   Y = rnorm(100),
#   Z = rpois(100, 5)
# )

# save_barplots(data, "X")

