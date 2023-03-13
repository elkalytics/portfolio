#' Fit a supervised regression or classification model
#' 
#' This function fits a supervised regression or classification model using the specified regression algorithm.
#' 
#' Note that this function is still in development.
#' 
#' @param x A data frame with predictor variables.
#' @param y A numeric vector (for regression) or a factor (for classification) with the target variable.
#' @param method A character string specifying the regression algorithm to use. Valid options are "lm" for linear regression, "glm" for generalized linear regression, "randomForest" for random forest, and "xgboost" for gradient boosting.
#' @param problem_type A character string specifying the type of problem to solve. Valid options are "regression" for regression and "classification" for classification. Default is "regression".
#' @param params A list of additional parameters to be passed to the regression algorithm. Default is an empty list.
#' @param train_prop A numeric value specifying the proportion of data to use for training. Default is 0.8.
#' @param seed An integer specifying the seed for the random number generator. Default is NULL.
#' @param cv_folds An integer specifying the number of cross-validation folds. Default is 5.
#'
#' @return The trained regression or classification model.
#'
#' @examples 
#' x <- data.frame(
#'   var1 = c(1, 2, 3, 4, 5),
#'   var2 = c(2, 4, 6, 8, 10),
#'   var3 = c(3, 6, 9, 12, 15),
#'   y = c(10, 20, 30, 40, 50)
#' )
#' 
#' model <- supervised_regression(x[,1:3], x$y, method = "lm")
#' 
#' new_data <- data.frame(
#'   var1 = c(6, 7, 8),
#'   var2 = c(12, 14, 16),
#'   var3 = c(18, 21, 24)
#' )
#' 
#' predictions <- predict(model, newdata = new_data)
#'
supervised_regression <- function(x, y, method = "lm", problem_type = "regression", params = list(), train_prop = 0.8, seed = NULL, cv_folds = 5) {
  
  # Check if input variables are valid
  if (!is.data.frame(x) | !(is.numeric(y) || is.factor(y))) {
    stop("Invalid input data types: predictors must be a data frame and target variable must be numeric for regression or a factor for classification")
  }
  
  # Check if the target variable has missing values
  if (any(is.na(y))) {
    stop("Target variable contains missing values")
  }
  
  # Check if the input method is valid
  if (!method %in% c("lm", "glm", "randomForest", "xgboost")) {
    stop("Invalid regression algorithm")
  }
  
  # Check if the problem type is valid
  if (!problem_type %in% c("regression", "classification")) {
    stop("Invalid problem type")
  }
  
  # Check for collinearity among predictor variables
  if (problem_type == "regression") {
    if (cor(x) > 0.9) {
      warning("High collinearity among predictor variables may affect model performance")
    }
  }
  
  # Check for validity of additional parameters specified
  if (length(params) > 0) {
    switch(method,
           "lm" = {
             if (!is.list(params) | length(params) > 0) {
               warning("Invalid additional parameters specified")
               params <- list()
             }
           },
           "glm" = {
             if (!is.list(params) | length(params) > 1 | !all(names(params) %in% c("family"))) {
               warning("Invalid additional parameters specified")
               params <- list()
             }
           },
           "randomForest" = {
             if (!is.list(params) | length(params) > 0) {
               warning("Invalid additional parameters specified")
               params <- list()
             }
           },
           "xgboost" = {
             if (!is.list(params) | length(params) > 1 | !all(names(params) %in% c("params"))) {
               warning("Invalid additional parameters specified")
               params <- list()
             }
           }
    )
  }
  
  # Check for validity of train_prop argument
  if (!is.numeric(train_prop) || train_prop <= 0 || train_prop >= 1) {
    stop("Invalid value for train_prop argument")
  }
  
  # Check for validity of seed argument
  if (!is.null(seed) && !is.integer(seed)) {
    stop("Invalid value for seed argument")
  }
  
  # Check for validity of cv_folds argument
  if (!is.integer(cv_folds) || cv_folds <= 1) {
    stop("Invalid value for cv_folds argument")
  }
  
  # Set seed for random number generator if specified
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Split the data into training and testing sets
  if (problem_type == "regression") {
    train_size <- floor(nrow(x) * train_prop)
    train_indices <- sample(seq_len(nrow(x)), size = train_size, replace = FALSE)
    x_train <- x[train_indices, ]
    y_train <- y[train_indices]
    x_test <- x[-train_indices, ]
    y_test <- y[-train_indices]
  } else {
    library(caret)
    split <- createDataPartition(y, p = train_prop, list = FALSE)
    x_train <- x[split, ]
    y_train <- y[split]
    x_test <- x[-split, ]
    y_test <- y[-split]
  }
  
  # Fit the regression model using the specified method
  if (method == "lm") {
    model <- lm(y_train ~ ., data = x_train)
  } else if (method == "glm") {
    model <- glm(y_train ~ ., data = x_train, family = params$family)
  } else if (method == "randomForest") {
    model <- randomForest(y_train ~ ., data = x_train, mtry = ncol(x_train) %/% 3, importance = TRUE, ntree = 500, nodesize = 5, do.trace = 0, ...)
  } else if (method == "xgboost") {
    library(xgboost)
    dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
    model <- xgb.train(params = params$params, data = dtrain, nrounds = 10)
  }
  
  # Make predictions on the test set
  if (problem_type == "regression") {
    predictions <- predict(model, newdata = x_test)
    rmse <- sqrt(mean((predictions - y_test)^2))
    r_squared <- cor(predictions, y_test)^2
    message("Root Mean Squared Error: ", rmse)
    message("R-squared: ", r_squared)
  } else {
    predictions <- predict(model, newdata = x_test, type = "class")
    accuracy <- mean(predictions == y_test)
    message("Accuracy: ", accuracy)
  }
  
  # Return the trained model
  return(model)
}