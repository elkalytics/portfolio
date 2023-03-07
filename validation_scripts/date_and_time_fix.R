#' Reformat date-time or date stamps
#'
#' This function takes a character string as input and attempts to parse it as a date-time
#' or date stamp using the lubridate package. If successful, it reformats the stamp as a
#' standard US date-time stamp ("MM/DD/YYYY HH:MM:SS AM/PM") or date stamp ("MM/DD/YYYY").
#'
#' @param x A character string representing a date-time or date stamp
#' @return A character string representing the reformatted date-time or date stamp, or NA if the input is not a valid date-time or date stamp
#' @examples
#' reformat_date_time("01/02/2023 04:30:15 PM")
#' reformat_date_time("01/02/2023")
#' reformat_date_time("2023/01/02 04:30:15 PM")
#' reformat_date_time("2023/01/02")
#'
#' @import lubridate
#' @export
# Load package
library(lubridate)
# Save function
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