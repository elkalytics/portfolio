# Scrape boxoffice numbers across multiple dates
library(rvest)

scrape_box_office_range <- function(start_date, end_date) {
  # Generate vector of dates between start and end dates
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  dates <- format(dates, "%Y/%m/%d")
  
  # Scrape box office data for each date and save to list
  box_office_data <- list()
  for (date in dates) {
    # Construct URL for the given date
    url <- paste0("https://www.the-numbers.com/box-office-chart/daily/", date)
    
    # Scrape the table using CSS selectors
    webpage <- read_html(url)
    table <- html_table(html_nodes(webpage, "table")[2], fill = TRUE)
    
    # Add table to list with date as name
    box_office_data[[date]] <- table
  }
  
  return(box_office_data)
}

## Example
box_office_list <- scrape_box_office_range("2023/02/01", "2023/02/03")
