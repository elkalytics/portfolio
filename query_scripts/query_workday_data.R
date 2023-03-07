#' Query Workday data
#'
#' @param tenant the tenant ID
#' @param service_name the name of the Workday service
#' @param access_token the access token for the Workday API
#' @param request_type the type of Workday API request
#' @param request_parameters a list of parameters for the Workday API request
#'
#' @return a data frame with the queried data
#'
#' @importFrom httr POST add_headers
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' # Replace these values with your own Workday API credentials and parameters
#' tenant <- "mytenant"
#' service_name <- "My_Service_Name"
#' access_token <- "my_access_token"
#' request_type <- "My_Request_Type"
#' request_parameters <- list()
#'
#' # Call the query_workday_data function
#' data <- query_workday_data(tenant, service_name, access_token, request_type, request_parameters)
#'
#' # Manipulate and analyze the data as needed
#' # For example, print the number of records returned
#' cat("Number of records: ", nrow(data), "\n")
#'
# Load packages
library(httr)
library(jsonlite)
# Save function
query_workday_data <- function(tenant, service_name, access_token, request_type, request_parameters) {
  # Construct the API request
  response <- POST(paste0("https://wd5-impl-services1.workday.com/ccx/service/", tenant, "/", service_name, "/v47.0"),
                   add_headers(Authorization = paste0("Bearer ", access_token),
                               "Content-Type" = "application/json"),
                   body = list(request = list(type = request_type,
                                              version = "v47.0",
                                              request_parameters)))
  
  # Parse the JSON response into a data frame
  data <- jsonlite::fromJSON(rawToChar(response$content))
  
  # Return the queried data as a data frame
  return(data)
}