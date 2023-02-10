library(forecast)

forecast_models <- function(data) {
  naive_model <- naive(data)
  ets_model <- ets(data)
  tbats_model <- tbats(data)
  arima_model <- auto.arima(data)
  
  list(naive = naive_model, ets = ets_model, tbats = tbats_model, arima = arima_model)
}

air_passengers_data <- AirPassengers

forecast_models_result <- forecast_models(air_passengers_data)

forecast_models_result
