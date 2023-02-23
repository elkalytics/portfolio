# Sampling function for simple and stratified with/without replacements

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

## Sample 50 rows from the 'iris' dataset
# simple_sample <- random_sampling(iris, 50)
# View results
# head(simple_sample)

## Sample 100 rows from the 'mtcars' dataset with replacement
# replacement_sample <- random_sampling(mtcars, 100, with_replacement = TRUE)
# View results
# head(replacement_sample)

## Stratified sample 25 rows from each species of the 'iris' dataset
# stratified_sample <- random_sampling(iris, 25, strata = "Species")
# View results
# head(stratified_sample)

## Sample 100 rows from the 'mtcars' dataset with replacement
# stratified_replacement_sample <- random_sampling(iris, 50, strata = "Species", with_replacement = TRUE)
# View results
# head(stratified_replacement_sample)