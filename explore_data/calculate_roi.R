#' Calculate annualized rate of return (ROI)
#'
#' Calculates the annualized rate of return (ROI) for an investment given the initial value, final value, and investment period.
#'
#' @param initial_value Numeric value indicating the initial value of the investment.
#' @param final_value Numeric value indicating the final value of the investment.
#' @param investment_period Numeric value indicating the length of the investment period.
#' @param period_type Character string indicating the units of the investment period.
#' @param group Character string indicating the group to calculate ROI by. Defaults to NULL, which calculates the overall ROI.
#'
#' @return Numeric value indicating the annualized rate of return as a decimal.
#' 
#' @examples
#' calculate_roi(initial_value = 1000, final_value = 1200, investment_period = 365, period_type = "days", group = "A")
#' calculate_roi(initial_value = 1000, final_value = 1200, investment_period = 365, period_type = "days") # overall ROI
#'
#' # create a sample dataset with multiple groups
#' data <- data.frame(group = rep(c("A", "B", "C"), each = 10),
#'                    initial_value = rnorm(30, mean = 1000, sd = 100),
#'                    final_value = rnorm(30, mean = 1200, sd = 100),
#'                    investment_period = rep(365, 30),
#'                    period_type = rep("days", 30))
#' 
#' # calculate ROI by group
#' result <- by(data, data$group, function(x) {
#'   calculate_roi(initial_value = x$initial_value[1],
#'                 final_value = x$final_value[1],
#'                 investment_period = x$investment_period[1],
#'                 period_type = x$period_type[1],
#'                 group = unique(x$group))
#' })
#' 
#' # convert the list of results to a data frame
#' result_df <- do.call(rbind, result)
#' 
#' # print the result
#' result_df
#' @export
calculate_roi <- function(initial_value, final_value, investment_period, period_type, group = NULL) {
  if (!is.numeric(initial_value) || !is.numeric(final_value) || !is.numeric(investment_period)) {
    stop("All inputs must be numeric")
  }
  
  if (initial_value <= 0 || final_value <= 0 || investment_period <= 0) {
    stop("All inputs must be positive")
  }
  
  if (initial_value >= final_value) {
    stop("Final value must be greater than initial value")
  }
  
  if (!period_type %in% c("days", "weeks", "months", "years")) {
    stop("Invalid period type. Must be one of 'days', 'weeks', 'months', or 'years'")
  }
  
  periods_per_year <- switch(period_type,
                             days = 365,
                             weeks = 52,
                             months = 12,
                             years = 1)
  
  n <- investment_period / periods_per_year
  
  r <- (final_value - initial_value) / initial_value
  
  if (is.null(group)) {
    return ((1 + r)^(1/n) - 1)
  } else {
    return(data.frame(group = group, roi = (1 + r)^(1/n) - 1))
  }
}