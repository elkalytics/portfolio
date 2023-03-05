#' Load required packages
#' 
#' This function loads the required packages for the analysis.
#' 
#' @importFrom randomForest randomForest
#' @importFrom caret createDataPartition
#' @importFrom tidyverse ggplot
#' @importFrom car vif
#' 
#' @examples 
#' library(randomForest)
#' library(caret)
#' library(tidyverse)
#' library(car)

library(randomForest)
library(caret)
library(tidyverse)
library(car)

#' Generate fake data
#' 
#' This function generates fake data for the analysis.
#' 
#' @param n The number of observations to generate.
#' 
#' @return A data frame containing the generated data.
#'
#' @examples 
#' set.seed(123)
#' mydata <- generate_fake_data(n = 1000)
#' 
#' trainIndex <- createDataPartition(mydata$y, p = .8, list = FALSE)
#' train <- mydata[trainIndex, ]
#' test <- mydata[-trainIndex, ]

generate_fake_data <- function(n) {
  set.seed(123)
  x1 <- rnorm(n, mean = 0, sd = 1)
  x2 <- rnorm(n, mean = 0, sd = 1)
  x3 <- rnorm(n, mean = 0, sd = 1)
  y <- 2 * x1 + 3 * x2 - 5 * x3 + rnorm(n, mean = 0, sd = 1)
  data.frame(x1, x2, x3, y)
}

mydata <- generate_fake_data(n = 1000)

trainIndex <- createDataPartition(mydata$y, p = .8, list = FALSE)
train <- mydata[trainIndex, ]
test <- mydata[-trainIndex, ]

#' Use random forest to perform feature selection
#' 
#' This function uses random forest to perform feature selection on the given dataset.
#' 
#' @param data A data frame containing the input variables and response variable.
#' @param response The name of the response variable.
#' @param importance A logical value indicating whether or not to compute variable importance measures.
#' 
#' @return A randomForest object containing the fitted model.
#'
#' @examples 
#' set.seed(123)
#' n <- 1000
#' mydata <- generate_fake_data(n)
#' 
#' trainIndex <- createDataPartition(mydata$y, p = .8, list = FALSE)
#' train <- mydata[trainIndex, ]
#' test <- mydata[-trainIndex, ]
#' 
#' rf_model <- perform_feature_selection(data = train, response = "y", importance = TRUE)
#' varImpPlot(rf_model, sort = TRUE, main = "Random Forest Feature Importance")

perform_feature_selection <- function(data, response, importance) {
  randomForest(as.formula(paste(response, "~ .")), data = data, importance = importance)
}

rf_model <- perform_feature_selection(data = train, response = "y", importance = TRUE)
varImpPlot(rf_model, sort = TRUE, main = "Random Forest Feature Importance")

#' Check linearity using a scatterplot
#' 
#' This function checks the linearity assumption using a scatterplot.
#' 
#' @param data A data frame containing the input variables and response variable.
#' @param x The name of the predictor variable to plot on the x-axis.
#' @param y The name of the response variable to plot on the y-axis.
#' 
#' @examples 
#' set.seed(123)
#' n <- 1000
#' mydata <- generate_fake_data(n)
#' 
#' trainIndex <- createDataPartition(mydata$y, p = .8, list = FALSE)
#' train <- mydata[trainIndex, ]
#' test <- mydata[-trainIndex, ]
#' 
#' check_linearity(train, "x1", "y")

check_linearity <- function(data, x, y) {
  ggplot(data, aes_string(x = x, y = y)) + 
    geom_point() + 
    ggtitle(paste0("Scatterplot of ", y, " vs. ", x)) + 
    xlab(x) + 
    ylab(y)
}

check_linearity(train, "x1", "y")

#' Check normality using a histogram
#' 
#' This function checks the normality assumption using a histogram.
#' 
#' @param data A data frame containing the response variable.
#' @param y The name of the response variable.
#' 
#' @examples 
#' set.seed(123)
#' n <- 1000
#' mydata <- generate_fake_data(n)
#' 
#' trainIndex <- createDataPartition(mydata$y, p = .8, list = FALSE)
#' train <- mydata[trainIndex, ]
#' test <- mydata[-trainIndex, ]
#' 
#' check_normality(train, "y")

check_normality <- function(data, y) {
  ggplot(data, aes_string(x = y)) +
    geom_histogram() +
    ggtitle(paste0("Histogram of ", y)) +
    xlab(y) +
    ylab("Count")
}

check_normality(train, "y")

#' Check normality with Q-Q plot
#' 
#' This function checks the normality assumption using a Q-Q plot.
#' 
#' @param residuals The residuals from the fitted model.
#' 
#' @examples 
#' set.seed(123)
#' n <- 1000
#' mydata <- generate_fake_data(n)
#' 
#' trainIndex <- createDataPartition(mydata$y, p = .8, list = FALSE)
#' train <- mydata[trainIndex, ]
#' test <- mydata[-trainIndex, ]
#' 
#' rf_model <- perform_feature_selection(data = train, response = "y", importance = TRUE)
#' residuals <- train$y - rf_model$predicted
#' 
#' check_normality_qq(residuals)

check_normality_qq <- function(residuals) {
  qqPlot(residuals, main = "Q-Q plot of residuals")
}

rf_model <- perform_feature_selection(data = train, response = "y", importance = TRUE)
residuals <- train$y - rf_model$predicted

check_normality_qq(residuals)

#' Check homoscedasticity using a residuals vs. fitted plot
#' 
#' This function checks the homoscedasticity assumption using a residuals vs. fitted plot.
#' 
#' @param data A data frame containing the response variable and predicted values.
#' @param predicted The name of the predicted variable.
#' @param residuals The name of the residuals variable.
#' 
#' @examples 
#' set.seed(123)
#' n <- 1000
#' mydata <- generate_fake_data(n)
#' 
#' trainIndex <- createDataPartition(mydata$y, p = .8, list = FALSE)
#' train <- mydata[trainIndex, ]
#' test <- mydata[-trainIndex, ]
#' 
#' rf_model <- perform_feature_selection(data = train, response = "y", importance = TRUE)
#' residuals <- train$y - rf_model$predicted
#' 
#' check_homoscedasticity(train, "y", "residuals")

check_homoscedasticity <- function(data, predicted, residuals) {
  ggplot(data, aes_string(x = predicted, y = residuals)) + 
    geom_point() + 
    ggtitle("Residuals vs. Fitted") + 
    xlab("Fitted values") + 
    ylab("Residuals")
}

check_homoscedasticity(train, "rf_model$predicted", "residuals")

#' Check multicollinearity using VIF
#' 
#' This function checks the multicollinearity assumption using VIF.
#' 
#' @param data A data frame containing the input variables and response variable.
#' @param formula A formula specifying the linear regression model to fit.
#' 
#' @examples 
#' set.seed(123)
#' n <- 1000
#' mydata <- generate_fake_data(n)
#' 
#' trainIndex <- createDataPartition(mydata$y, p = .8, list = FALSE)
#' train <- mydata[trainIndex, ]
#' test <- mydata[-trainIndex, ]
#' 
#' check_multicollinearity(train, y ~ x1 + x2 + x3)

check_multicollinearity <- function(data, formula) {
  vif_values <- vif(lm(formula, data = data))
  vif_table <- tibble(Variable = names(vif_values), VIF = vif_values)
  print(vif_table)
}

check_multicollinearity(train, y ~ x1 + x2 + x3)

