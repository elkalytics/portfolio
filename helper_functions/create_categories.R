#' Make categories of specified variable and grouping
#'
#' @param x A numeric vector to categorize
#' @param n The number of groups to divide the data into
#' @return A factor vector with the specified number of categories
#'
#' @examples 
#' data <- data.frame(x = rnorm(100))
#' categories <- create_categories(data$x, 4)
#' table(categories)
#'
#' @export 
create_categories <- function(x, n) {
  breaks <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  categories <- cut(x, breaks = breaks, labels = paste0("Group", 1:n))
  return(categories)
}