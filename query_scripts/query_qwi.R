#' Retrieve Quarterly Workforce Indicators (QWI) data for specified years, states, and endpoints.
#'
#' This function retrieves QWI data for specified years, states, and endpoints using the Census API. 
#' The data is returned as a list with each element containing the data for one endpoint.
#'
#' This function is still in development.
#'
#' @param start_year The first year for which to retrieve data.
#' @param end_year The last year for which to retrieve data.
#' @param states A character vector of state abbreviations for which to retrieve data. Defaults to "all".
#' @param endpoints A character vector of endpoints to retrieve. The possible values are "sa", "se", and "rh".
#' @param census_key A character string containing your Census API key.
#' 
#' @import tidyqwi
#' 
#' @return A list containing the QWI data for each endpoint.
#' 
#' @examples
#' # Retrieve QWI data for all states and all endpoints for 2010 and 2011
#' query_qwi <- get_qwi_data(start_year = 2010, end_year = 2011, states = "all", census_key = "YOUR_CENSUS_API_KEY")
#' 
#' # Retrieve QWI data for specified states and endpoints for 2015
#' query_qwi <- get_qwi_data(start_year = 2015, end_year = 2015, states = c("TX", "CA"), endpoints = c("sa", "se"), census_key = "YOUR_CENSUS_API_KEY")
#' 
# Load packages
library(tidyqwi)
# Save function
query_qwi <- function(start_year, end_year, states = "all", endpoints = c("sa", "se", "rh"), census_key) {
  years <- as.character(start_year:end_year)
  if (states == "all") {
    states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", 
                "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
                "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  }
  qwi_data_list <- lapply(endpoints, function(endpoint) {
    endpoint_data_list <- list()
    for (year in years) {
      for (state in states) {
        qwi_data <- get_qwi(years = year,
                            states = state,
                            geography = "county",
                            apikey = census_key,
                            endpoint = endpoint,
                            variables = c("sEmp", "Emp"),
                            all_groups = FALSE,
                            industry_level = "2",
                            processing = "sequential")
        endpoint_data_list[[paste0(year, "_", state)]] <- qwi_data
      }
    }
    endpoint_data <- do.call(rbind, endpoint_data_list)
    return(endpoint_data)
  })
  names(qwi_data_list) <- endpoints
  return(qwi_data_list)
}