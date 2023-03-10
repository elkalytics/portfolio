#' impute_reg function
#' 
#' This function imputes missing values of a variable using a linear regression model.
#' 
#' @param data a data frame containing the variables to be imputed
#' @param var the name of the variable to be imputed
#' @param predictors a vector of predictor variable names
#' 
#' @return a data frame with imputed values for the variable
#' 
#' @examples 
#' data <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100), w = rnorm(100))
#' data[1:10, "x"] <- NA
#' imputed_data <- impute_reg(data, "x", c("y", "z", "w"))
#' head(imputed_data)
#' 
#' @export
impute_reg <- function(data, var, predictors) {
  # Split data into complete and incomplete cases
  complete <- data[complete.cases(data), ]
  incomplete <- data[!complete.cases(data), ]
  
  # Fit a regression model to predict missing values
  formula <- as.formula(paste(var, "~", paste(predictors, collapse = "+")))
  model <- lm(formula, data = complete)
  
  # Predict missing values using the regression model
  incomplete[var] <- predict(model, newdata = incomplete[, predictors])
  
  # Combine complete and imputed data
  imputed <- rbind(complete, incomplete)
  
  return(imputed)
}