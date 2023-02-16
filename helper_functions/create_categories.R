# Function to make categories of specified variable and grouping

create_categories <- function(x, n) {
  breaks <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  categories <- cut(x, breaks = breaks, labels = paste0("Group", 1:n))
  return(categories)
}

## create a sample data set
# data <- data.frame(x = rnorm(100))

## create 4 categories based on the distribution of x
# categories <- create_categories(data$x, 4)

## view the resulting categories
# table(categories)


