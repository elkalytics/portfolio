# Save function
cross_validate_regression <- function(data, formula, k = 5) {
  
  # Shuffle the data randomly
  shuffled_data <- data[sample(nrow(data)),]
  
  # Split the data into k folds
  folds <- cut(seq(1, nrow(shuffled_data)), breaks = k, labels = FALSE)
  
  # Create empty vectors to store the results
  mse <- rep(0, k)
  r_squared <- rep(0, k)
  
  # Perform k-fold cross-validation
  for (i in 1:k) {
    
    # Get the test and train indices for this fold
    test_indices <- which(folds == i)
    train_indices <- which(folds != i)
    
    # Split the data into test and train sets
    test_data <- shuffled_data[test_indices, ]
    train_data <- shuffled_data[train_indices, ]
    
    # Fit a linear regression model to the training data
    X_train <- model.matrix(formula, data = train_data)
    y_train <- train_data$y
    beta_hat <- solve(t(X_train) %*% X_train) %*% t(X_train) %*% y_train
    
    # Make predictions on the test set
    X_test <- model.matrix(formula, data = test_data)
    y_test <- test_data$y
    predictions <- X_test %*% beta_hat
    
    # Calculate the mean squared error and R-squared for this fold
    mse[i] <- mean((y_test - predictions)^2)
    r_squared[i] <- summary(lm(formula, data = train_data))$r.squared
  }
  
  # Return the average MSE and R-squared across all folds
  return(list(avg_mse = mean(mse), avg_r_squared = mean(r_squared)))
}


## Create a data frame with three columns
# df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))

## Add some noise to the y values
# df$y <- df$y + 2 * df$x1 + 3 * df$x2 + rnorm(100)

## Specify formula
# formula <- y ~ x1 + x2

## Cross validate
# results <- cross_validate_regression(df, formula, k = 5)

## View results
# results
