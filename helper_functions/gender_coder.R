# Load the gender package
library(gender)

# Define a function that uses the gender package to estimate gender
get_gender <- function(name) {
  # Estimate gender using the gender function from the gender package
  gender_obj <- gender(name)
  
  # Extract the estimated gender from the gender object
  gender <- gender_obj$gender
  
  # Return the estimated gender
  return(gender)
}

## Example usage
# get_gender("Alice")
# get_gender("Bob")
# get_gender("Taylor")