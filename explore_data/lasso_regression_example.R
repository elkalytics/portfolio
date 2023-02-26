library(glmnet)
library(caret)

# Load a dataset
data(iris)

# Convert factor variable to numeric
iris$Species <- as.numeric(iris$Species)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = .8, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Fit a Lasso regression model
lassoModel <- cv.glmnet(as.matrix(trainData[, -5]), trainData$Species,
                        alpha = 1, nfolds = 5)

# Extract variable importance scores
lassoImp <- coef(lassoModel, s = "lambda.min")
lassoImp <- abs(lassoImp)[-1]

# Sort by importance
lassoImp <- data.frame(Importance = sort(lassoImp, decreasing = TRUE))
