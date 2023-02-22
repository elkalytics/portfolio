# Grid search ARIMA
# The auto.arima forecast function does a stepwise selection
# This script shows how you can iterate through all possible parameters

library(forecast)

# Load the AirPassengers data
data("AirPassengers")

# Split the data into training and test sets
train <- window(AirPassengers, end = c(1958, 12))
test <- window(AirPassengers, start = c(1959, 1))

# Define the grid of parameter values to search
p_values <- c(0, 1, 2)
d_values <- c(0, 1)
q_values <- c(0, 1, 2)
P_values <- c(0, 1, 2)
D_values <- c(0, 1)
Q_values <- c(0, 1, 2)

# Initialize a variable to store the best model
best_model <- NULL
best_aic <- Inf

# Loop over all possible parameter combinations
for (p in p_values) {
  for (d in d_values) {
    for (q in q_values) {
      for (P in P_values) {
        for (D in D_values) {
          for (Q in Q_values) {
            
            # Fit the model
            model <- auto.arima(train, start.p = p, d = d, start.q = q,
                                max.p = max(p_values), max.d = max(d_values), max.q = max(q_values),
                                seasonal = TRUE, start.P = P, D = D, start.Q = Q,
                                max.P = max(P_values), max.D = max(D_values), max.Q = max(Q_values),
                                stepwise = FALSE, approximation = FALSE, trace = FALSE)
            
            # Calculate the AIC
            aic <- AIC(model)
            
            # Update the best model if this one has a lower AIC
            if (aic < best_aic) {
              best_model <- model
              best_aic <- aic
            }
          }
        }
      }
    }
  }
}

# Make a forecast using the best model
forecast <- forecast(best_model, h = 12)

# Plot the forecast
plot(forecast, main = "AirPassengers Forecast")

# Calculate the forecast accuracy on the test set
accuracy <- accuracy(forecast, test)

# Print the best model and its AIC
print(best_model)
cat("Best AIC:", best_aic, "\n")

# Print the forecast accuracy
print(accuracy)
