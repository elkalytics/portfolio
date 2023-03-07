#' Random Sampling
#'
#' This function performs simple or stratified random sampling with or without replacement
#' from a given data frame.
#'
#' @param data A data frame
#' @param size An integer indicating the number of rows to sample
#' @param strata A string indicating the name of the variable to use as the stratum,
#'   or NULL for simple random sampling (default = NULL)
#' @param with_replacement A logical indicating whether to sample with replacement (default = FALSE)
#'
#' @return A data frame containing the sampled rows
#'
#' @examples
#' # Simple random sampling from the 'iris' dataset
#' simple_sample <- random_sampling(iris, 50)
#' head(simple_sample)
#'
#' # Stratified random sampling from the 'iris' dataset
#' stratified_sample <- random_sampling(iris, 25, strata = "Species")
#' head(stratified_sample)
#'
#' @export
# Save function
random_sampling <- function(data, size, strata = NULL, with_replacement = FALSE) {
  if (!is.null(strata)) {
    # Stratified random sampling
    samples <- vector("list", length(strata))
    for (i in seq_along(strata)) {
      stratum_data <- data[data[[strata[i]]] == unique(data[[strata[i]]]), ]
      if (with_replacement) {
        samples[[i]] <- stratum_data[sample(nrow(stratum_data), size, replace = TRUE), ]
      } else {
        samples[[i]] <- stratum_data[sample(nrow(stratum_data), size), ]
      }
    }
    return(do.call("rbind", samples))
  } else {
    # Simple random sampling
    if (with_replacement) {
      return(data[sample(nrow(data), size, replace = TRUE), ])
    } else {
      return(data[sample(nrow(data), size), ])
    }
  }
}