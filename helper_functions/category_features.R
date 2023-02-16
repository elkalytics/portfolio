# Construct new categorical variables

create_categories <- function(data) {
  for (var in names(data)[sapply(data, is.numeric)]) {
    for (n in 2:10) {
      breaks <- quantile(jitter(data[[var]], factor = 1e-6), probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
      categories <- cut(data[[var]], breaks = breaks, labels = paste0(var, "_Group", 1:n))
      data[paste0(var, "_Group", n)] <- categories
    }
  }
  return(data)
}


## create a sample data set
# data <- data.frame(x = rnorm(100), y = rnorm(100), z = runif(100), w = rpois(100, 1))

## create new variables for 2 through 10 categories of every numeric variable
# data <- create_categories(data)

## view the updated data frame
# head(data)
