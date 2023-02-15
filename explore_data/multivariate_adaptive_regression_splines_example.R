# Load required libraries
library(earth)
library(caret)

# Load data
data(mtcars)

# Split data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(mtcars$mpg, p = 0.8, list = FALSE)
train <- mtcars[trainIndex, ]
test <- mtcars[-trainIndex, ]

# Define the grid of hyperparameters to search over
hyper_params <- expand.grid(degree = c(1, 2, 3),
                            nprune = c(5, 10, 15))

# Define the control parameters for the model training
ctrl <- trainControl(method = "cv", number = 5, search = "grid")

# Train the model using grid search for hyperparameters
set.seed(123)
mars_model <- train(mpg ~ ., data = train, method = "earth", trControl = ctrl,
                    tuneGrid = hyper_params)

# Print the best hyperparameters found by the grid search
mars_model$bestTune

# Make predictions on test set
pred <- predict(mars_model, newdata = test)

# Calculate the RMSE of the predictions
sqrt(mean((test$mpg - pred)^2))
