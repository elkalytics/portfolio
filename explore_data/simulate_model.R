#' Simulate a linear regression model
#'
#' Given a dataset and a set of predictor variables, this function fits a linear regression model to
#' predict a response variable, and then creates a simulation of the model using the predictor variables.
#'
#' @param data A data frame containing the variables to use in the model.
#' @param response_var A string giving the name of the response variable in \code{data}.
#' @param predictor_vars A character vector giving the names of the predictor variables in \code{data}.
#' @return A data frame containing the predictor variables and the predicted response variable for the simulated data.
#' @examples
#' simulate_model(mtcars, "mpg", c("wt", "cyl"))
#' @importFrom tidyverse mutate
#' @importFrom broom tidy
#' @importFrom stats lm predict
simulate_model <- function(data, response_var, predictor_vars) {
  # Define the model formula
  model_formula <- as.formula(paste(response_var, "~", paste(predictor_vars, collapse="+")))
  
  # Fit the model
  model <- lm(model_formula, data = data)
  
  # Create the simulation data
  simulation_data <- data.frame(lapply(data[, predictor_vars], function(x) seq(from = min(x), to = max(x), length.out = nrow(data))))
  
  # Create the simulation
  forecast_values <- predict(model, newdata = simulation_data)
  
  # Visualize the results
  result <- simulation_data %>%
    mutate(forecasted_value = forecast_values)
  
  return(result)
}