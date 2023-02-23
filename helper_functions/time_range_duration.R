# Calculate the distance between two time stamps "10:00 AM - 9:00 PM"
# Works on characters saved and appends duration of range to data frame as seconds, minutes, hours, days, weeks, months, quarters, and years
# Corrected so it works on AM/PM values that cross midnight and omits NA

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


## Example usage:
# data <- data.frame(id = 1:5, time_range = c("9:00 AM - 10:00 AM", "9:00 PM - 3:00 AM", "8:30 AM - 12:15 PM", NA, "7:00 PM - 11:30 PM"))
# data <- time_range_duration(data, "time_range")

## Review
# head(data)
