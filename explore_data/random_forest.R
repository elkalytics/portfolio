# Load the required packages
library(randomForest)
library(caret)
library(ggplot2)

# Generate a fake dataset with 3 classes
set.seed(123)
n <- 1000
x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 0, 1)
x3 <- rnorm(n, 0, 1)
y <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
data <- data.frame(x1, x2, x3, y)

# Check for missing values
sum(is.na(data))

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$y, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Make sure factor levels match between train and test
levels(test$y) <- levels(train$y)

# Build the random forest model
set.seed(123)
rf_model <- randomForest(y ~ ., data = train, ntree = 500, mtry = sqrt(ncol(train)-1))

# Check the variable importance
varImpPlot(rf_model)

# Make predictions on the test set
rf_pred <- predict(rf_model, newdata = test)

# Evaluate the model performance
confusionMatrix(rf_pred, test$y)
