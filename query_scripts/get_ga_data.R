#' Scrape website traffic data from Google Analytics
#' 
#' @param view_id The view ID of the Google Analytics account to pull data from
#' @param start_date The start date for the date range to pull data from, in YYYY-MM-DD format
#' @param end_date The end date for the date range to pull data from, in YYYY-MM-DD format
#' @param endpoint The endpoint for the Google Analytics API
#' @param secrets A list containing the client ID and client secret for the Google Analytics API
#' 
#' @return A data frame containing the date and number of users for each day in the date range
#' 
#' @importFrom httr oauth_service_token POST content config
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom ggplot2 aes geom_line labs
#' @importFrom base format as.Date as.numeric
#' 
#' @examples 
#' # Example usage: get traffic data for last 30 days
#' data <- get_ga_data(view_id = "your-view-id", 
#'                     start_date = format(Sys.Date() - 30, "%Y-%m-%d"), 
#'                     end_date = format(Sys.Date(), "%Y-%m-%d"),
#'                     endpoint = "https://oauth2.googleapis.com/token",
#'                     secrets = list(client_id = "your-client-id", client_secret = "your-client-secret"))
#' 
#' # Create line chart of daily traffic
#' ggplot(data, aes(x = date, y = users)) +
#'   geom_line() +
#'   labs(x = "Date", y = "Users", title = "Website Traffic Over Time")
# Load packages
library(rvest)
library(httr)
library(jsonlite)
library(ggplot2)
# Save function
get_ga_data <- function(view_id, start_date, end_date, endpoint, secrets) {
  
  # Authenticate with Google Analytics API
  ga_auth <- httr::oauth_service_token(endpoint, secrets, scope = "https://www.googleapis.com/auth/analytics.readonly")
  
  # Build API query
  api_url <- "https://analyticsreporting.googleapis.com/v4/reports:batchGet"
  query_body <- list(
    reportRequests = list(
      list(
        viewId = view_id,
        dateRanges = list(list(startDate = start_date, endDate = end_date)),
        metrics = list(list(expression = "ga:users")),
        dimensions = list(list(name = "ga:date")),
        includeEmptyRows = TRUE
      )
    )
  )
  
  # Make API request and parse response
  api_response <- httr::POST(api_url, body = jsonlite::toJSON(query_body), httr::config(token = ga_auth))
  api_response <- jsonlite::fromJSON(httr::content(api_response, "text"))
  
  # Extract and format data
  data <- api_response$reports[[1]]$data$rows
  data <- data.frame(date = as.Date(data$dimensions), users = as.numeric(data$metrics))
  
  return(data)
}