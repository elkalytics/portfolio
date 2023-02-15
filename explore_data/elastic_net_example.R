library(glmnet)
data(iris)

# Convert the response variable to a numeric variable
iris$Species <- as.numeric(iris$Species)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(nrow(iris), 0.7 * nrow(iris))
train <- iris[train_index, ]
test <- iris[-train_index, ]

# Preprocess the data
train_x <- as.matrix(train[, -5])
train_y <- train[, 5]

test_x <- as.matrix(test[, -5])
test_y <- test[, 5]

# Set up the grid of alpha and lambda values to search over
alphas <- seq(0, 1, 0.1)
lambdas <- 10^seq(-3, 3, by = 0.1)

# Fit the elastic net model with cross-validation for each alpha value
cv_model <- lapply(alphas, function(alpha) {
  cv.glmnet(train_x, train_y, alpha = alpha, lambda = lambdas)
})

# Extract the cross-validation errors for each alpha value
cv_errors <- sapply(cv_model, function(cv) {
  cv$cvm
})

# Find the optimal alpha value
optimal_alpha <- alphas[which.min(colMeans(cv_errors))]

# Find the optimal lambda value for the optimal alpha value
optimal_lambda <- cv_model[[which(optimal_alpha == alphas)]]$lambda.min

# Fit the elastic net model with the optimal alpha and lambda values
enet_model <- glmnet(train_x, train_y, alpha = optimal_alpha, lambda = optimal_lambda)

# Get the coefficients of the model
coef_enet <- coef(enet_model)

# Identify the non-zero coefficients
selected_features <- which(coef_enet != 0)

# Get the column names of the enet_model
enet_colnames <- colnames(enet_model$beta)

# Create a new selected_features vector that only includes valid indices
selected_features <- which(colnames(test_x) %in% enet_colnames)

# Subset the test set to the selected features, if any
if (length(selected_features) > 0) {
  selected_test_x <- test_x[, selected_features, drop = FALSE]
  
  # Make predictions on the test set using the selected features
  predictions <- predict(enet_model, newx = selected_test_x)
} else {
  predictions <- NULL
}

# Calculate the test error
if (!is.null(predictions)) {
  test_error <- mean((predictions - test_y)^2)
} else {
  test_error <- mean((mean(train_y) - test_y)^2)
}

# Print the test error and the selected features
cat("Optimal alpha:", optimal_alpha, "\n")
cat("Optimal lambda:", optimal_lambda, "\n")
cat("Test error:", test_error, "\n")
if (length(selected_features) > 0) {
  cat("Selected features:", colnames(selected_test_x))
} else {
  cat("No features selected")
}
