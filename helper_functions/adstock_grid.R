# Grid search function to be used with adstock function
# Searches for optimal parameters

# Define a function to calculate the sum of squared errors between the predicted and actual values
sse <- function(actual, predicted) {
  sum((actual - predicted)^2)
}

# Define a function to perform the grid search for optimal lag and decay values
adstock_grid_search <- function(x, lags, decays) {
  sse_values <- matrix(nrow = length(lags), ncol = length(decays))
  for (i in 1:length(lags)) {
    for (j in 1:length(decays)) {
      adstocked <- adstock(x, lags[i], decays[j])
      sse_values[i,j] <- sse(x, adstocked)
    }
  }
  results <- list(lags = lags, decays = decays, sse = sse_values)
  return(results)
}

# Example usage
x <- c(10, 20, 30, 40, 50)
lags <- seq(1, 3, by = 1)
decays <- seq(0.1, 0.9, by = 0.1)
results <- adstock_grid_search(x, lags, decays)

# Print the results of the grid search
print(results)

# Find the optimal lag and decay values that minimize SSE
min_sse <- min(results$sse)
optimal_indices <- which(results$sse == min_sse, arr.ind = TRUE)
optimal_lag <- results$lags[optimal_indices[1]]
optimal_decay <- results$decays[optimal_indices[2]]
cat("Optimal lag:", optimal_lag, "\n")
cat("Optimal decay:", optimal_decay, "\n")