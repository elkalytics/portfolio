#' Look up address details
#'
#' This function takes an address as input and returns a data frame containing
#' various details about the address, including the street, city, county, state,
#' ZIP code, FIPS code, latitude, and longitude. If possible, the function also
#' determines the level of detail for map visualization (either the state or county
#' FIPS code) and returns the corresponding shapefile. The function tries to geocode
#' the address using the `geocode` function from the `ggmap` package. If the `geocode`
#' function is unable to find a match for the address, the function tries again with
#' the `source` parameter set to "google". If the `google` source is still unable to
#' find a match, the function parses the address and tries geocoding again. If the
#' address still cannot be geocoded, the function returns an empty data frame.
#'
#' @param address A character string representing the address to look up
#'
#' @return A data frame containing various details about the address, including the
#' street, city, county, state, ZIP code, FIPS code, latitude, and longitude, as well
#' as the level of detail for map visualization (if available) and the source used to
#' obtain the details. If a shapefile is available for the address, the data frame also
#' includes a `shapefile` column containing the path to the shapefile.
#'
#' @importFrom ggmap geocode
#' @importFrom stringr str_to_lower gsub str_extract
#' @importFrom tigris state_shapefile county_shapefile
#' @export
#'
#' @examples
#' address <- "1600 Pennsylvania Avenue NW, Washington, DC 20500"
#' get_address_details(address) # Returns a data frame with details for the White House
# Load packages
library(ggmap)
library(stringr)
library(tigris)
get_address_details <- function(address) {
  
  # Initializing variables to store the details
  street <- ""
  city <- ""
  county <- ""
  state <- ""
  zip <- ""
  fips <- ""
  lat <- ""
  lon <- ""
  map_level <- ""
  source <- ""
  shapefile <- ""
  
  # Checking if the given address is valid
  address_check <- geocode(as.character(address), output = "more")
  
  if (nrow(address_check) > 0) {
    # Extracting details from the geocoded address
    street <- address_check$street
    city <- address_check$locality
    county <- address_check$county
    state <- address_check$administrative_area_level_1
    zip <- address_check$postal_code
    fips <- address_check$fips_county
    lat <- address_check$lat
    lon <- address_check$lon
    
    # Determining the level of detail for map visualization
    if (is.na(fips)) {
      map_level <- state
    } else {
      map_level <- fips
    }
    
    # Checking if shapefile is available for the given address
    if (map_level == state) {
      # Obtaining state shapefile
      tryCatch({
        shapefile <- state_shapefile(state)
      }, error = function(e) {
        shapefile <- "NA"
      })
    } else if (map_level == fips) {
      # Obtaining county shapefile
      tryCatch({
        shapefile <- county_shapefile(fips)
      }, error = function(e) {
        shapefile <- "NA"
      })
    } else {
      shapefile <- "NA"
    }
    
    # Marking the source as Geocode
    source <- "Geocode"
    
  } else {
    
    # Geocoding the address using ggmap
    address_ggmap <- geocode(as.character(address), output = "more", source = "google")
    
    if (nrow(address_ggmap) > 0) {
      # Extracting details from the geocoded address
      street <- address_ggmap$street
      city <- address_ggmap$locality
      county <- address_ggmap$county
      state <- address_ggmap$administrative_area_level_1
      zip <- address_ggmap$postal_code
      fips <- address_ggmap$fips_county
      lat <- address_ggmap$lat
      lon <- address_ggmap$lon
      
      # Determining the level of detail for map visualization
      if (is.na(fips)) {
        map_level <- state
      } else {
        map_level <- fips
      }
      
      # Checking if shapefile is available for the given address
      if (map_level == state) {
        # Obtaining state shapefile
        tryCatch({
          shapefile <- state_shapefile(state)
        }, error = function(e) {
          shapefile <- "NA"
        })
      } else if (map_level == fips) {
        # Obtaining county shapefile
        tryCatch({
          shapefile <- county_shapefile(fips)
          }, error = function(e) {
            shapefile <- "NA"
          })
      } else {
        shapefile <- "NA"
      }
      
      # Marking the source as Geocode
      source <- "Geocode"
      
    } else {
      
      # Parsing the address if it is not valid
      address_parsed <- str_to_lower(gsub("[[:punct:]]", "", as.character(address)))
      
      # Extracting the street number and name
      address_parsed_split <- strsplit(address_parsed, " ")[[1]]
      street_number <- str_extract(address_parsed_split, "^\\d+")
      street_name <- paste(address_parsed_split[-1], collapse = " ")
      
      # Geocoding the parsed address
      address_parsed_ggmap <- geocode(paste(street_number, street_name, sep = " "), output = "more", source = "google")
      
      if (nrow(address_parsed_ggmap) > 0) {
        # Extracting details from the geocoded address
        street <- address_parsed_ggmap$street
        city <- address_parsed_ggmap$locality
        county <- address_parsed_ggmap$county
        state <- address_parsed_ggmap$administrative_area_level_1
        zip <- address_parsed_ggmap$postal_code
        fips <- address_parsed_ggmap$fips_county
        lat <- address_parsed_ggmap$lat
        lon <- address_parsed_ggmap$lon
        
        # Determining the level of detail for map visualization
        if (is.na(fips)) {
          map_level <- state
        } else {
          map_level <- fips
        }
        
        # Checking if shapefile is available for the given address
        if (map_level == state) {
          # Obtaining state shapefile
          tryCatch({
            shapefile <- state_shapefile(state)
          }, error = function(e) {
            shapefile <- "NA"
          })
        } else if (map_level == fips) {
          # Obtaining county shapefile
          tryCatch({
            shapefile <- county_shapefile(fips)
          }, error = function(e) {
            shapefile <- "NA"
          })
        } else {
          shapefile <- "NA"
        }
        
        # Marking the source as Parsed Geocode
        source <- "Parsed Geocode"
        
      } else {
        
        # Marking the source as Address
        source <- "Address"
        
      }
      
    }
    
  }
  
  # Returning the details, source, and shapefile
  return(data.frame(street = street, city = city, county = county, state = state, zip = zip, fips = fips, lat = lat, lon = lon, map_level = map_level, source = source, shapefile = shapefile))
}