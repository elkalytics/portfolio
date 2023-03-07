#' Convert time stamps to different units
#'
#' This function takes two time stamps and calculates the difference between
#' them in various units such as seconds, minutes, hours, days, weeks, months,
#' quarters, and years.
#'
#' @param start_time A character string of the starting time in any format that
#' can be parsed by the as.POSIXct function.
#'
#' @param end_time A character string of the ending time in any format that can
#' be parsed by the as.POSIXct function.
#'
#' @return A named vector of time differences.
#'
#' @examples
#' timestamp1 <- "2023-02-01 09:30:00"
#' timestamp2 <- "2023-02-22 16:45:00"
#' time_difference_calculator(timestamp1, timestamp2)
#'
#' @export
# Save function
time_difference_calculator <- function(start_time, end_time) {
  # Convert start and end times to POSIXct format
  start_time <- as.POSIXct(start_time, tz = "UTC")
  end_time <- as.POSIXct(end_time, tz = "UTC")
  
  # Calculate difference in seconds
  time_diff <- difftime(end_time, start_time, units = "secs")
  
  # Calculate time difference in various units
  seconds <- round(time_diff, 0)
  minutes <- round(time_diff / 60, 0)
  hours <- round(time_diff / 3600, 0)
  days <- round(time_diff / 86400, 0)
  weeks <- round(time_diff / 604800, 0)
  
  # Account for leap years in year, month, and quarter calculations
  start_year <- as.integer(format(start_time, "%Y"))
  end_year <- as.integer(format(end_time, "%Y"))
  start_month <- as.integer(format(start_time, "%m"))
  end_month <- as.integer(format(end_time, "%m"))
  year_diff <- end_year - start_year
  month_diff <- (end_year - start_year) * 12 + (end_month - start_month)
  quarter_diff <- ceiling(month_diff / 3)
  if (month_diff < 0) {
    year_diff <- year_diff - 1
    month_diff <- month_diff + 12
    quarter_diff <- quarter_diff - 1
  }
  
  # Calculate years, months, and quarters
  years <- year_diff
  months <- month_diff
  quarters <- quarter_diff
  
  # Adjust time difference for months and quarters
  days_in_start_month <- as.integer(format(as.Date(start_time), "%d"))
  days_in_end_month <- as.integer(format(as.Date(end_time), "%d"))
  days_in_start_quarter <- as.integer(format(as.Date(start_time), "%j"))
  days_in_end_quarter <- as.integer(format(as.Date(end_time), "%j"))
  if (days_in_start_month == days_in_end_month) {
    # No adjustment needed for months
  } else if (days_in_start_month > days_in_end_month) {
    months <- months - 1
    if (months < 0) {
      years <- years - 1
      months <- months + 12
    }
  }
  if (days_in_start_quarter == days_in_end_quarter) {
    # No adjustment needed for quarters
  } else if (days_in_start_quarter > days_in_end_quarter) {
    quarters <- quarters - 1
    if (quarters < 0) {
      years <- years - 1
      quarters <- quarters + 4
    }
  }
  
  # Return a named vector of time differences
  result <- c(seconds = seconds, minutes = minutes, hours = hours, days = days, weeks = weeks, 
              months = months, quarters = quarters, years = years)
  return(result)
}