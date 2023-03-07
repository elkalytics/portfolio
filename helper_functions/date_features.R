#' Create features from dates
#'
#' @param date A character vector of dates
#' @param date_format A character vector specifying the format of the date input, defaults to "%Y-%m-%d"
#' @return A data frame containing various features extracted from the date input
#'
#' @importFrom lubridate parse_date_time format quarter week wday leap_year
#' @importFrom tis isHoliday isBusinessDay
#'
#' @examples 
#' date_features("2023-03-03")
#'
#' date_features("03/03/2023", date_format = "%m/%d/%Y")
#'
#' date_features("2023-03")
#'
#' @export 
# Load packages
library(lubridate)
library(tidyverse)
library(tis)
# Pre-calculate days in month and year
month_lengths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
year_lengths <- rep(c(365, 366), each = 365)
# Function for date feature engineering
date_features <- function(date, date_format = "%Y-%m-%d") {
  
  # Add default day value of 1 if necessary
  if (length(strsplit(date, "-")[[1]]) == 2) {
    date <- paste0(date, "-01")
  }
  
  # Convert date to specified format
  date <- lubridate::parse_date_time(date, orders = date_format)
  
  # Extract year and month
  year <- as.numeric(format(date, "%Y"))
  month <- as.numeric(format(date, "%m"))
  
  # Extract day of the week if available, otherwise set to NA
  ifelse(is.na(wday(date, label = TRUE)),
         day_of_week <- NA,
         day_of_week <- wday(date, label = TRUE))
  
  # Extract day of the month if available, otherwise set to NA
  ifelse(is.na(format(date, "%d")),
         day_of_month <- NA,
         day_of_month <- as.numeric(format(date, "%d")))
  
  # Extract day of the year if available, otherwise set to NA
  ifelse(is.na(format(date, "%j")),
         day_of_year <- NA,
         day_of_year <- as.numeric(format(date, "%j")))
  
  # Calculate week of the month if available, otherwise set to NA
  ifelse(is.na(day_of_month),
         week_of_month <- NA,
         week_of_month <- ceiling(day_of_month / 7))
  
  # Calculate week of the year if available, otherwise set to NA
  ifelse(is.na(week(date)),
         week_of_year <- NA,
         week_of_year <- as.numeric(week(date)))
  
  # Calculate day of the quarter if available, otherwise set to NA
  ifelse(is.na(day_of_year),
         day_of_quarter <- NA,
         day_of_quarter <- as.numeric(day_of_year - (as.numeric(quarter(date, with_year = TRUE)) - 1) * 91))
  
  # Calculate month of the quarter if available, otherwise set to NA
  ifelse(is.na(month),
         month_of_quarter <- NA,
         month_of_quarter <- as.numeric(ceiling(month / 3)))
  
  # Calculate quarter of the year if available, otherwise set to NA
  ifelse(is.na(quarter(date, with_year = FALSE)),
         quarter_of_year <- NA,
         quarter_of_year <- as.numeric(quarter(date, with_year = FALSE)))
  
  # Calculate day of week as a percentage of that week if available, otherwise set to NA
  ifelse(is.na(day_of_month),
         day_of_week_pct <- NA,
         day_of_week_pct <- day_of_month / 7)
  
  # Calculate day of month as a percentage of that month if available, otherwise set to NA
  ifelse(is.na(day_of_month) | is.na(month_lengths[month]),
         day_of_month_pct <- NA,
         day_of_month_pct <- day_of_month / month_lengths[month])
  
  # Calculate day of year as a percentage of that year if available, otherwise set to NA
  ifelse(is.na(day_of_year) | is.na(year_lengths[leap_year(year) + 1]),
         day_of_year_pct <- NA,
         day_of_year_pct <- day_of_year / year_lengths[leap_year(year) + 1])
  
  # Check if date is a US holiday
  is_holiday <- tis::isHoliday(date, businessOnly = TRUE)
  
  # Check if date is a US business day
  is_business <- tis::isBusinessDay(date)
  
  # Return result as a data frame
  data.frame(
    year = year,
    year_month = format(date, "%Y-%m"),
    month = month,
    day_of_week = as.character(day_of_week),
    week_of_month = week_of_month,
    week_of_year = week_of_year,
    day_of_quarter = day_of_quarter,
    quarter_of_year = quarter_of_year,
    month_of_quarter = month_of_quarter,
    day_of_year_pct = day_of_year_pct,
    day_of_week_pct = day_of_week_pct,
    day_of_month_pct = day_of_month_pct,
    is_holiday = as.character(is_holiday),
    is_business = as.character(is_business)
  )
}