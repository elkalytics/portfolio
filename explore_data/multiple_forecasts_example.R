#' Forecasting Models
#'
#' This function fits several forecasting models to a given time series data.
#' 
#' @param data A univariate time series object.
#'
#' @return A list containing fitted models for Naive, ETS, TBATS, and ARIMA.
#'
#' @details This function fits four different forecasting models to a given time series data:
#' naive, ets, tbats, and auto.arima. The fitted models are returned as a list.
#'
#' @examples 
#' air_passengers_data <- AirPassengers
#' forecast_models_result <- forecast_models(air_passengers_data)
#' 
#' @import forecast
#' @export
forecast_models <- function(data) {
  library(forecast)
  
  naive_model <- naive(data)
  ets_model <- ets(data)
  tbats_model <- tbats(data)
  arima_model <- auto.arima(data)
  
  list(naive = naive_model, ets = ets_model, tbats = tbats_model, arima = arima_model)
}