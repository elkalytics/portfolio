#' An R function that queries the metadata from a salesforce report
#' For salesforce API version 52.0 or higher
#'
#' @param report_id The ID of the Salesforce report
#' @param access_token The access token for the Salesforce API
#'
#' @return A dataframe containing the metadata fields for the report
#'
#' @import httr
#' @import rjson
#' @importFrom utils stopForStatus
#'
#' @examples
#' query_salesforce_report_metadata("00Oxxxxxxxxxxxxxxx", "xxxxxxxxxxxxxxxxxxxx")
query_salesforce_report_metadata <- function(report_id, access_token) {
  library(httr)
  library(rjson)
  
  # Create the URL for the metadata query
  instance <- "yourinstance"  # replace with your Salesforce instance name
  url <- paste0("https://", instance, ".salesforce.com/services/data/v52.0/analytics/reports/",
                report_id, "/metadata")
  
  # Set up the API request
  headers <- c(Authorization = paste0("Bearer ", access_token),
               Accept = "application/json")
  response <- GET(url, add_headers(headers))
  
  # Check if the API request was successful
  stop_for_status(response)
  
  # Parse the JSON response and extract the metadata fields
  metadata <- fromJSON(content(response, "text"))
  fields <- c("label", "name", "type", "format", "groupingLevel")
  
  # Save the metadata as a dataframe
  metadata_df <- data.frame(matrix(ncol=length(fields), nrow=length(metadata)))
  colnames(metadata_df) <- fields
  
  for (i in seq_along(metadata)) {
    metadata_df[i,] <- as.data.frame(t(unlist(metadata[[i]][fields])), stringsAsFactors = FALSE)
  }
  
  return(metadata_df)
}