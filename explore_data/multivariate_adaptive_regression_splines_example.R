#' Fit a MARS model to predict mpg in mtcars dataset.
#'
#' This script illustrates how to fit a Multivariate Adaptive Regression Splines (MARS) model
#' to a dataset to predict an outcome (miles per gallon).
#'
#' @param data a data frame containing the data
#' @param degree the maximum degree of interactions between variables
#' @param nprune the minimum number of observations in a terminal node
#' @param method the modeling method to use, "earth" for MARS
#' @param trControl a trainControl object that controls the training process
#' @param tuneGrid a data frame specifying the grid of hyperparameters to search over
#'
#' @return a trained MARS model object
#'
#' @examples
#' data(mtcars)
#' mars_model <- fit_mars_model(mtcars, degree = 2, nprune = 5)
#' 
#' @importFrom earth earth
#' @importFrom caret createDataPartition train trainControl
#' 
#' @export
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
