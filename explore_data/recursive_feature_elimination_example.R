#' Load and preprocess iris data for Recursive Feature Elimination
#' 
#' This script illustrates how to perform Recursive Feature Elimination (RFE) on the training set using the Random Forest algorithm.
#' 
#' @importFrom caret createDataPartition rfeControl rfe rfFuncs
#' @importFrom datasets iris
#' 
#' @return A data frame containing the variable importance scores computed from the RFE analysis.
#' 
#' @examples
#' data <- iris_rfe()
#' 
#' @export
# Load package
library(caret)

# Load a dataset
data(iris)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = .8, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Recursive Feature Elimination (RFE)
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
rfeProfile <- rfe(x = trainData[, -5], y = trainData[, 5], sizes = c(1:4),
                  rfeControl = ctrl)

# Get the variable importance scores
rfeImp <- rfeProfile$variables
