library(ggplot2)

plot_smooth_group <- function(data, x, y, color, facet, method) {
  ggplot(data, aes_string(x, y, color = color)) +
    geom_point() +
    geom_smooth(method = method, se = TRUE) +
    facet_grid(paste(". ~", facet))
}

# Example usage:
plot_smooth_group(mpg, "displ", "hwy", color = NULL, facet = "cyl", method = "lm")
plot_smooth_group(mpg, "displ", "hwy", color = NULL, facet = "cyl", method = "loess")
plot_smooth_group(mpg, "displ", "hwy", color = NULL, facet = "cyl", method = "gam")
plot_smooth_group(mpg, "displ", "hwy", color = NULL, facet = "cyl", method = "glm")
