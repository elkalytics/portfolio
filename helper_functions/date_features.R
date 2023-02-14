
# Load packages
library(lubridate)
library(tidyverse)
library(tis)

# Function for date feature engineering
date_features <- function(date, date_format = "%Y-%m-%d") {
  
  # Convert date to YYYY-MM-DD format if necessary
  date <- as.Date(strptime(date, format = date_format))
  date_format <- "%Y-%m-%d"
  
  # Extract year, month, day of the week, day of the month, day of the year
  year <- as.numeric(format(date, "%Y"))
  month <- as.numeric(format(date, "%m"))
  day_of_week <- wday(date, label = TRUE)
  day_of_month <- as.numeric(format(date, "%d"))
  day_of_year <- as.numeric(format(date, "%j"))
  
  # Calculate week of the month, week of the year, and day of the quarter
  week_of_month <- ceiling(day_of_month / 7)
  week_of_year <- as.numeric(week(date))
  day_of_quarter <- as.numeric(day_of_year - (as.numeric(quarter(date, with_year = TRUE)) - 1) * 91)
  
  # Calculate month of the quarter and quarter of the year
  month_of_quarter <- as.numeric(ceiling(month / 3))
  quarter_of_year <- as.numeric(quarter(date, with_year = FALSE))
  
  # Calculate day of week as a percentage of that week
  day_of_week_pct <- day_of_month / 7
  
  # Calculate day of month as a percentage of that month
  month_length <- as.numeric(days_in_month(date))
  day_of_month_pct <- day_of_month / month_length
  
  # Calculate day of year as a percentage of that year
  year_length <- as.numeric(ifelse(leap_year(year), 366, 365))
  day_of_year_pct <- day_of_year / year_length
  
  # Check if date is a US holiday
  is_holiday <- isHoliday(date, businessOnly = TRUE)
  
  # Check if date is a US business day
  is_business <- isBusinessDay(date)
  
  # Return result as a data frame
  data.frame(
    year = year,
    year_month = format(date, "%Y-%m"),
    month = month,
    day_of_week = day_of_week,
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



## Example
# date_features("2023-02-14")
