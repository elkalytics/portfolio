# Load required packages
library(randomForest)
library(caret)
library(tidyverse)
library(car)

# Generate fake data
set.seed(123)
n <- 1000
x1 <- rnorm(n, mean = 0, sd = 1)
x2 <- rnorm(n, mean = 0, sd = 1)
x3 <- rnorm(n, mean = 0, sd = 1)
y <- 2 * x1 + 3 * x2 - 5 * x3 + rnorm(n, mean = 0, sd = 1)
mydata <- data.frame(x1, x2, x3, y)

# Separate your data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(mydata$y, p = .8, list = FALSE)
train <- mydata[trainIndex, ]
test <- mydata[-trainIndex, ]

# Use random forest to perform feature selection
rf_model <- randomForest(y ~ ., data = train, importance = TRUE)
varImpPlot(rf_model, sort = TRUE, main = "Random Forest Feature Importance")

# Check assumptions
# Check linearity using a scatterplot
ggplot(train, aes(x = x1, y = y)) + 
  geom_point() + 
  ggtitle("Scatterplot of y vs. x1") + 
  xlab("x1") + 
  ylab("y")

ggplot(train, aes(x = x2, y = y)) + 
  geom_point() + 
  ggtitle("Scatterplot of y vs. x2") + 
  xlab("x2") + 
  ylab("y")

ggplot(train, aes(x = x3, y = y)) + 
  geom_point() + 
  ggtitle("Scatterplot of y vs. x3") + 
  xlab("x3") + 
  ylab("y")

# Check normality using a histogram
ggplot(train, aes(x = y)) +
  geom_histogram() +
  ggtitle("Histogram of y") +
  xlab("y") +
  ylab("Count")

# Check normality with Q-Q plot
residuals <- train$y - rf_model$predicted
qqPlot(residuals, main = "Q-Q plot of residuals")

# Check homoscedasticity using a residuals vs. fitted plot
ggplot(train, aes(x = rf_model$predicted, y = residuals)) + 
  geom_point() + 
  ggtitle("Residuals vs. Fitted") + 
  xlab("Fitted values") + 
  ylab("Residuals")

# Check multicollinearity using VIF
vif_values <- vif(lm(y ~ x1 + x2 + x3, data = train))
vif_table <- tibble(Variable = c("x1", "x2", "x3"), VIF = vif_values)
print(vif_table)
