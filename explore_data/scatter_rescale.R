library(ggplot2)

plot_rescaled <- function(x, y){
  # Rescale the variables so they have similar scales centered at 0
  x_rescaled <- (x - mean(x)) / sd(x)
  y_rescaled <- (y - mean(y)) / sd(y)
  
  # Create the scatter plot with a loess smooth
  ggplot(data.frame(x_rescaled, y_rescaled), aes(x_rescaled, y_rescaled)) +
    geom_point() +
    geom_smooth(method = "loess") +
    geom_hline(yintercept = 0, linewidth = 1) +
    geom_vline(xintercept = 0, linewidth = 1) +
    xlab("Rescaled x") +
    ylab("Rescaled y") +
    coord_equal()
}


# Generate example data
# x <- rnorm(100)
# y <- x + rnorm(100, mean = 0, sd = 0.5)

# Call the function
# plot_rescaled(x, y)
