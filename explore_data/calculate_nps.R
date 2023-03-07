#' Calculates the Net Promoter Score (NPS)
#' 
#' This function takes a vector of ratings ranging from 1 to 10 and calculates the NPS, a customer loyalty metric that measures how likely a customer is to recommend a product or service to others.
#' 
#' @param ratings A vector of numerical values between 1 and 10 representing customer ratings
#' @param group A vector of grouping variables used to calculate NPS by group, default = NULL for overall NPS
#' 
#' @return The Net Promoter Score (NPS) as a numerical value between -100 and 100, or a list of NPS values by group if a group variable is specified
#' 
#' @examples
#' # Calculate overall NPS
#' ratings <- c(10, 8, 7, 6, 9, 5, 8, 9, 10, 7)
#' nps <- calculate_nps(ratings)
#' cat("Overall Net Promoter Score:", round(nps, 2), "\n")
#'
#' # Calculate NPS by group
#' ratings <- c(10, 8, 7, 6, 9, 5, 8, 9, 10, 7)
#' group <- c("A", "A", "B", "B", "A", "B", "A", "B", "A", "B")
#' nps_by_group <- calculate_nps(ratings, group)
#' print(nps_by_group)
#' 
#' @export
calculate_nps <- function(ratings, group=NULL) {
  
  # Check input is a vector of numerical values between 1 and 10
  if (!is.numeric(ratings) || any(ratings < 1) || any(ratings > 10)) {
    stop("Input must be a vector of numerical values between 1 and 10.")
  }
  
  # If no group variable is specified, calculate overall NPS
  if (is.null(group)) {
    promoters <- sum(ratings >= 9)
    detractors <- sum(ratings <= 6)
    total <- length(ratings)
    nps <- 100 * (promoters - detractors) / total
    return(nps)
  }
  
  # If group variable is specified, calculate NPS by group
  else {
    if (!is.factor(group)) {
      group <- factor(group)
    }
    nps_by_group <- tapply(ratings, group, function(x) {
      promoters <- sum(x >= 9)
      detractors <- sum(x <= 6)
      total <- length(x)
      nps <- 100 * (promoters - detractors) / total
      return(nps)
    })
    return(nps_by_group)
  }
}