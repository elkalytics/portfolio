# Load the required library
library(tidyverse)
library(lmtest)

# Generate fake data
set.seed(123) # for reproducibility
x <- runif(100, 0, 10)
y <- 2*x + rnorm(100, 0, 1)
data <- data.frame(x = x, y = y)

# Create a linear regression model
model <- lm(y ~ x, data)

# Calculate the sensitivity of the model to changes in x
sensitivity <- function(x) {
  new_data <- data.frame(x = x)
  new_prediction <- predict(model, newdata = new_data)
  return(new_prediction)
}

# Conduct the sensitivity analysis for x values ranging from 1 to 10
x_values <- seq(1, 10, by = 0.1)
predictions <- sensitivity(x_values)

# Check the lengths of the x_values and predictions vectors
if (length(x_values) != length(predictions)) {
  stop("Error: x_values and predictions vectors have different lengths.")
}

# Plot the results
plot(x_values, predictions, type = "l", xlab = "x", ylab = "y")

# Check correlation
cor(data$x, data$y)

####################################### Checking assumptions

# Create a scatter plot with regression line
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "x", y = "y") +
  theme_bw()

# Create a residual plot for checking assumption of linearity
model <- lm(y ~ x, data)
residuals <- resid(model)
ggplot(data, aes(x = fitted(model), y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Predicted y", y = "Residuals") +
  theme_bw()

# Create a histogram of the residuals for normality
ggplot(data, aes(x = resid(model))) +
  geom_histogram(bins = 20) +
  labs(x = "Residuals", y = "Count") +
  theme_bw()

# Perform the Shapiro-Wilk test for normality
shapiro.test(resid(model))

# Perform the Breusch-Pagan test for constant variance
bptest(model)
