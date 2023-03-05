#' Load the required packages
#'
#' @import randomForest
#' @import caret
#' @import ggplot2
#'
#' @export
library(randomForest)
library(caret)
library(ggplot2)

#' Generate a fake dataset with 3 classes
#'
#' @param n Number of samples
#' @return A data frame with n rows and 4 columns, including x1, x2, x3, and y
#'
#' @export
set.seed(123)
n <- 1000
x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 0, 1)
x3 <- rnorm(n, 0, 1)
y <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
data <- data.frame(x1, x2, x3, y)

#' Check for missing values
#'
#' @param data The data frame to check for missing values
#' @return The number of missing values
#'
#' @export
sum(is.na(data))

#' Split the data into training and testing sets
#'
#' @param data The data frame to split
#' @param p The proportion of the data to use for training
#' @return A list of two data frames: the training set and the testing set
#'
#' @export
set.seed(123)
trainIndex <- createDataPartition(data$y, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

#' Make sure factor levels match between train and test
#'
#' @param train The training set
#' @param test The testing set
#'
#' @export
levels(test$y) <- levels(train$y)

#' Build the random forest model
#'
#' @param formula A formula specifying the model
#' @param data The data frame containing the data to fit the model
#' @param ntree The number of trees to grow
#' @param mtry The number of variables to sample as candidates at each split
#' @return A random forest model object
#'
#' @export
set.seed(123)
rf_model <- randomForest(y ~ ., data = train, ntree = 500, mtry = sqrt(ncol(train)-1))

#' Check the variable importance
#'
#' @param model The random forest model
#'
#' @export
varImpPlot(rf_model)

#' Make predictions on the test set
#'
#' @param model The random forest model
#' @param newdata The data frame containing the new data to predict on
#' @return A vector of predicted classes
#'
#' @export
rf_pred <- predict(rf_model, newdata = test)

#' Evaluate the model performance
#'
#' @param actual The actual classes
#' @param predicted The predicted classes
#' @return A confusion matrix object
#'
#' @export
confusionMatrix(rf_pred, test$y)
