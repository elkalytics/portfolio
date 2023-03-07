# Load the gender package
#' Estimate the gender of a given name
#'
#' This function uses the `gender` package to estimate the gender of a given name.
#'
#' @param name A character string representing a person's name
#'
#' @return A character string representing the estimated gender ("male", "female",
#' or "unknown")
#'
#' @importFrom gender gender
#' @export
#'
#' @examples
#' get_gender("Alice") # Returns "female"
#' get_gender("Bob") # Returns "male"
library(gender)
# Save function
get_gender <- function(name) {
  # Estimate gender using the gender function from the gender package
  gender_obj <- gender(name)
  
  # Extract the estimated gender from the gender object
  gender <- gender_obj$gender
  
  # Return the estimated gender
  return(gender)
}