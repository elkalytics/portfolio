#' Calculate the duration between two time stamps in various units
#'
#' This function takes a data frame and a column containing time ranges as input
#' and calculates the duration between the start and end times of each range in
#' seconds, minutes, and hours. The duration is appended to the data frame as
#' new columns.
#'
#' @param data A data frame containing the time range column.
#'
#' @param time_range_col A character string or numeric index of the time range
#' column in the data frame.
#'
#' @return The input data frame with new columns appended for the duration of
#' each time range in seconds, minutes, and hours.
#'
#' @examples
#' data <- data.frame(id = 1:5, time_range = c("9:00 AM - 10:00 AM", "9:00 PM - 3:00 AM", "8:30 AM - 12:15 PM", NA, "7:00 PM - 11:30 PM"))
#' data <- time_range_duration(data, "time_range")
#' head(data)
#'
#' @export
# Save function
time_range_duration <- function(data, time_range_col) {
  # Get the index of the time range column.
  if (is.numeric(time_range_col)) {
    col_index <- time_range_col
  } else {
    col_index <- match(time_range_col, names(data))
  }
  
  # Only process rows where time_range is not NA
  non_na_rows <- !is.na(data[[col_index]])
  
  # Split the input time range into its start and end times.
  times <- strsplit(data[[col_index]][non_na_rows], " - ")
  
  # Parse the start and end times into POSIXct objects.
  start_time <- as.POSIXct(sapply(times, `[[`, 1), format = "%I:%M %p", tz = "UTC")
  end_time <- as.POSIXct(sapply(times, `[[`, 2), format = "%I:%M %p", tz = "UTC")
  
  # Create a vector to add days to end_time if it's earlier than start_time.
  # We add 1 day if end_time is earlier than start_time to account for a time range that crosses over midnight.
  days <- ifelse(end_time < start_time, 1, 0)
  
  # Calculate the duration between the start and end times.
  duration_seconds <- as.numeric(difftime(end_time + days*86400, start_time, units = "secs"))
  
  # Convert the duration to various time units.
  seconds <- round(duration_seconds, digits = 0)
  minutes <- round(seconds / 60, digits = 0)
  hours <- round(duration_seconds / 3600, digits = 0)
  for (suffix in suffixes) {
    col_name <- paste0(names(data)[col_index], suffix)
    data[non_na_rows, col_name] <- switch(suffix,
                                          "_seconds" = seconds,
                                          "_minutes" = minutes,
                                          "_hours" = hours)
  }
  
  # Return the updated data set.
  return(data)
}