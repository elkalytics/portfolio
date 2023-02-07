library(lubridate)

reformat_date_time <- function(x) {
  if (is.na(x)) return(NA)
  # Try to parse the input as a date-time stamp
  y <- dmy_hms(x)
  if (!is.na(y)) {
    # Reformat to a standard US date-time stamp
    return(format(y, "%m/%d/%Y %I:%M:%S %p"))
  } else {
    # Try to parse the input as a date stamp
    y <- dmy(x)
    if (!is.na(y)) {
      # Reformat to a standard US date stamp
      return(format(y, "%m/%d/%Y"))
    } else {
      # Return NA if unable to parse as date-time or date stamp
      return(NA)
    }
  }
}
