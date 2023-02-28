# Stepwise regression with cv
# Select direction

# Save function
stepwise_regression <- function(data, dependent_var, step_type = "both"){
  
  # Extract the dependent variable name as a string
  dep_var <- deparse(substitute(dependent_var))
  
  # Build formula object
  formula <- reformulate(".", response = dep_var)
  
  # Fit the full model 
  full.model <- lm(formula, data = data)
  
  # Stepwise regression model
  step.model <- stepAIC(full.model, direction = step_type, 
                        trace = FALSE)
  
  # Summary of stepwise regression model
  summary(step.model)
  
  # Regression model using regsubsets function
  models <- regsubsets(formula, data = data, nvmax = 5,
                       method = "seqrep")
  
  # Summary of regression models
  summary(models)
  
  # Set seed for reproducibility
  set.seed(123)
  # Set up repeated k-fold cross-validation
  train.control <- trainControl(method = "cv", number = 10)
  # Train the model
  step.model <- train(formula, data = data,
                      method = "leapBackward", 
                      tuneGrid = data.frame(nvmax = 1:5),
                      trControl = train.control
  )
  # Results of the model
  result <- step.model$results
  best_tune <- step.model$bestTune
  final_model_summary <- summary(step.model$finalModel)
  coefficients <- coef(step.model$finalModel, 4)
  
  # Return the results
  return(list(result = result, best_tune = best_tune, 
              final_model_summary = final_model_summary, 
              coefficients = coefficients))
}


## Example data
# data(swiss)

## Example use
# stepwise_regression(swiss, Fertility, "backward")
# stepwise_regression(swiss, Fertility, "forward")
# stepwise_regression(swiss, Fertility, "both")


