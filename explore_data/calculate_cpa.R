#' Calculate cost per acquisition (CPA)
#' 
#' This function calculates the cost per acquisition (CPA) given ad spend and conversions for a specific group (if provided).
#' 
#' @param spend A numeric value representing the ad spend.
#' @param conversions A numeric value representing the number of conversions.
#' @param group (Optional) A vector of factors representing the groups to calculate CPA for. If not provided, the function will calculate the overall CPA.
#' 
#' @return A numeric vector representing the calculated CPA(s), or 0 if conversions is 0.
#' 
#' @examples
#' # Calculate CPA for all data with $1000 spend and 50 conversions
#' cpa_all <- calculate_cpa(1000, 50)
#' 
#' # Print overall CPA
#' print(cpa_all)
#' 
#' # Calculate CPA for two groups with different spends and conversions
#' group <- c("A", "B", "A")
#' spend <- c(1000, 2000, 1500)
#' conversions <- c(50, 80, 60)
#' 
#' cpa_group <- calculate_cpa(spend, conversions, group)
#' 
#' # Print group-level CPAs
#' print(cpa_group)
#' 
#' @export
calculate_cpa <- function(spend, conversions, group = NULL) {
  if (is.null(group)) {
    # Calculate overall CPA
    if (conversions == 0) {
      return(0)
    } else {
      return(spend / conversions)
    }
  } else {
    # Calculate CPA by group
    unique_groups <- unique(group)
    cpas <- numeric(length(unique_groups))
    
    for (i in seq_along(unique_groups)) {
      group_spend <- spend[group == unique_groups[i]]
      group_conversions <- conversions[group == unique_groups[i]]
      
      if (sum(group_conversions) == 0) {
        cpas[i] <- 0
      } else {
        cpas[i] <- sum(group_spend) / sum(group_conversions)
      }
    }
    
    names(cpas) <- unique_groups
    return(cpas)
  }
}
