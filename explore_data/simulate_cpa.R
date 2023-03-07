#' Simulate cost per acquisition (CPA)
#' 
#' This function simulates a range of cost per acquisition (CPA) values given a range of ad spend and conversions.
#' 
#' @param spend_range A numeric vector representing the range of ad spend values.
#' @param conversions_range A numeric vector representing the range of conversions values.
#' @param group A vector specifying the group membership for each combination of spend and conversions. If NULL, the function calculates the CPA range for all data without grouping.
#' 
#' @return A list with two components:
#'   - cpas: A list of matrices where each matrix represents the calculated CPA for a given combination of spend and conversions within each group.
#'   - range: A data.frame containing the minimum and maximum CPA for each group.
#' 
#' @examples
#' # Simulate CPAs for a range of ad spend and conversions values
#' spend_range <- seq(0, 1000, by = 100)
#' conversions_range <- seq(0, 100, by = 10)
#' group <- rep(1:2, each = length(spend_range) * length(conversions_range) / 2)
#' cpas <- simulate_cpa(spend_range, conversions_range, group)
#' 
#' # Print CPAs for each group
#' for (i in 1:nlevels(factor(group))) {
#'   cat(paste0("Group ", i, ":\n"))
#'   print(cpas$cpas[[i]])
#' }
#' 
#' # Print CPA range for each group
#' print(cpas$range)
#' 
#' @export
simulate_cpa <- function(spend_range, conversions_range, group = NULL) {
  n_groups <- length(unique(group))
  cpas <- vector("list", n_groups)
  for (k in 1:n_groups) {
    cpas[[k]] <- matrix(nrow = length(spend_range), ncol = length(conversions_range))
    for (i in 1:length(spend_range)) {
      for (j in 1:length(conversions_range)) {
        spend <- spend_range[i]
        conversions <- conversions_range[j]
        if (conversions == 0) {
          cpas[[k]][i,j] <- 0
        } else {
          cpas[[k]][i,j] <- spend / conversions
        }
      }
    }
  }
  if (is.null(group)) {
    # Calculate CPA range for all data
    range <- data.frame(min = min(unlist(cpas)), max = max(unlist(cpas)))
  } else {
    # Calculate CPA range for each group
    range <- data.frame(group = unique(group), min = numeric(n_groups), max = numeric(n_groups))
    for (k in 1:n_groups) {
      range[k, "min"] <- min(cpas[[k]])
      range[k, "max"] <- max(cpas[[k]])
    }
  }
  return(list(cpas = cpas, range = range))
}