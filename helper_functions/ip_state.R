# Find state based on ip
# ipinfo might be out of date

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

## Use function
# ip_state("8.8.8.8")
# ip_state("216.58.194.174")

