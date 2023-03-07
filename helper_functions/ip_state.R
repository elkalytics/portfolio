#' Get state name from IP address
#'
#' This function takes an IP address as input and returns the name of the state that
#' the IP address is associated with, as determined by the IPinfo.io API.
#'
#' @param ip_address A character string representing the IP address to look up
#'
#' @return A character string representing the name of the state that the IP address
#' is associated with, as determined by the IPinfo.io API.
#'
#' @importFrom httr GET content
#' @export
#'
#' @examples
#' ip_state("8.8.8.8")
#' ip_state("216.58.194.174")
# Load library
library(httr)
# Save function
ip_state <- function(ip_address) {
  # Construct the URL for the IPinfo.io API
  url <- paste0("https://ipinfo.io/", ip_address, "/geo")
  
  # Make a GET request to the IPinfo.io API
  response <- GET(url)
  
  # Extract the state name from the response
  state <- content(response)$region
  
  # Return the state name
  return(state)
}