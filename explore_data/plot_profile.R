# Function to plot mean and SD across variables

# Save function
plot_profile <- function(df) {
  # Calculate the means and standard deviations of the columns in the data frame
  means <- apply(df, 2, mean)
  sds <- apply(df, 2, sd)
  
  # Create a plot of the means and standard deviations
  plot(means, type = "b", ylim = range(c(means - 2*sds, means + 2*sds)), 
       ylab = "Mean +/- 2SD", xlab = "",
       main = "Mean +/- 2SD plot of variables")
  
  # Add error bars to the plot to show the standard deviations
  arrows(x0 = 1:length(means), y0 = means - 2*sds, y1 = means + 2*sds, 
         angle = 90, code = 3, length = 0.1)
  
  # Remove the x-axis tick marks and values
  axis(1, at = 1:length(means), labels = FALSE, tick = FALSE)
  
  # Add the variable names as x-axis labels
  axis(1, at = 1:length(means), labels = names(df), line = 2.5)
  
  # Add a horizontal line at y=0 for visual reference
  abline(h=0, lty=2, col="gray")
  
  # Add a legend explaining the error bars
  legend("topleft", legend="Mean +/- 2SD", lty=1, pch=21, 
         pt.bg="white", pt.cex=1.2, bty="n", cex=0.8)
}

## Create a sample data frame
# df <- data.frame(
#   variable1 = rnorm(50, mean = 0, sd = 1),
#   variable2 = rnorm(50, mean = 2, sd = 2),
#   variable3 = rnorm(50, mean = -2, sd = 0.5)
# )

## Call the plot_profile function to create the plot
# plot_profile(df)
