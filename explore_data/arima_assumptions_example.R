
# Load the required packages
library(tseries)
library(forecast)

# Load the AirPassengers dataset
data(AirPassengers)

# Check the structure of the dataset
str(AirPassengers)

# Plot the time series data
plot(AirPassengers, main = "AirPassengers Data")


###################################################### Check assumptions

# Perform ADF for stationarity
adf_result <- adf.test(AirPassengers)

# View results
adf_result

# Perform KPSS for stationarity
kpss_result <- kpss.test(AirPassengers)

# View results
kpss_result


# Plot the autocorrelation function of the residuals
acf_resid <- acf(resid(arima(AirPassengers, order = c(1,1,1))), main = "Autocorrelation of Residuals")

# Perform Ljung-Box test for autocorrelation
lb_test <- Box.test(resid(arima(AirPassengers, order = c(1,1,1))), lag = 12, type = "Ljung-Box")

# View results of Ljung-Box test
lb_test

# Create a normal probability plot of the residuals
qqnorm_resid <- qqnorm(resid(arima(AirPassengers, order = c(1,1,1))), main = "Normal Probability Plot of Residuals")

# Create histogram
hist(resid(arima(AirPassengers, order = c(1,1,1))), main = "Histogram of Residuals")

# Perform Shapiro-Wilk test for normality
sw_test <- shapiro.test(resid(arima(AirPassengers, order = c(1,1,1))))

# View normality
sw_test


###################################################### Dealing with violations of autocorrelations

### Difference
# Take first difference to remove autocorrelation
diff_data <- diff(AirPassengers)
plot(diff_data, main = "Differenced AirPassengers Data")

# Fit an ARIMA model to the differenced data
arima_diff <- arima(diff_data, order = c(1, 0, 1))

### Add more lagged terms
# Fit an ARIMA model with more lagged terms
arima_lagged <- arima(AirPassengers, order = c(3, 0, 3))

### Try alternative models
# Fit a seasonal ARIMA model
sarima_model <- auto.arima(AirPassengers, seasonal = TRUE)

# Forecast future values
forecast_data <- forecast(sarima_model, h = 12)
plot(forecast_data, main = "Forecasted AirPassengers Data")


###################################################### Dealing with violations of stationarity

# Take first differences of the time series data
diff_AirPassengers <- diff(AirPassengers)

# Plot the differenced data
plot(diff_AirPassengers, main = "AirPassengers Data - First Differences")

# Perform ADF and KPSS tests for stationarity on the differenced data
adf_diff_result <- adf.test(diff_AirPassengers)
kpss_diff_result <- kpss.test(diff_AirPassengers)
cat("ADF test p-value (diff):", adf_diff_result$p.value, "\n")
cat("KPSS test p-value (diff):", kpss_diff_result$p.value, "\n")

# Fit an ARIMA model to the differenced data
arima_diff <- arima(diff_AirPassengers, order = c(1,0,1))

# Forecast future values using the ARIMA model and the differenced data
forecast_diff <- forecast(arima_diff, h = 24)

# Convert the forecasted differenced data back to the original scale
forecast_AirPassengers <- diffinv(forecast_diff$mean, xi = AirPassengers[length(AirPassengers)])

# Plot the forecasted values
plot(forecast_AirPassengers, main = "Forecasted AirPassengers Data")


###################################################### Dealing with violations of normality

# Transform the data using a log transformation
log_AirPassengers <- log(AirPassengers)

# Fit an ARIMA model to the transformed data
arima_log <- arima(log_AirPassengers, order = c(1,0,1))

# Extract the residuals from the transformed ARIMA model
log_residuals <- residuals(arima_log)

# Plot a histogram and a normal probability plot of the transformed residuals
par(mfrow = c(1,2))
hist(log_residuals, breaks = 20, main = "Histogram of Transformed Residuals")
qqnorm(log_residuals, main = "Normal Probability Plot of Transformed Residuals")

# Test for normality of the transformed residuals using the Shapiro-Wilk test
shapiro_log <- shapiro.test(log_residuals)
cat("Shapiro-Wilk test p-value (log transformed):", shapiro_log$p.value, "\n")

