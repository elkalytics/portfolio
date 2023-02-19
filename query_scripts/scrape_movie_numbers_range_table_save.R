# Scrape box office numbers across multiple dates
library(rvest)
library(dplyr)

# Scrape daily tables across start and end dates
scrape_box_office_range <- function(start_date, end_date) {
  # Generate vector of dates between start and end dates
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  dates <- format(dates, "%Y/%m/%d")
  
  # Scrape box office data for each date and save to list
  box_office_data <- data.frame()
  for (i in seq_along(dates)) {
    # Construct URL for the given date
    url <- paste0("https://www.the-numbers.com/box-office-chart/daily/", dates[i])
    
    # If the year changes, add an extra 30 second pause
    if (i > 1 && substr(dates[i], 1, 4) != substr(dates[i-1], 1, 4)) {
      Sys.sleep(30)
    }
    
    # Try to scrape the table using CSS selectors up to 5 times in case of a timeout
    tries <- 1
    while (tries <= 5) {
      tryCatch({
        webpage <- read_html(url)
        table <- html_table(html_nodes(webpage, "table")[2], fill = TRUE)
        # Convert list to data frame and add table date column
        table_df <- as.data.frame(table) %>%
          mutate(table_date = dates[i])
        # Bind table to previous tables
        box_office_data <- bind_rows(box_office_data, table_df)
        # Exit loop if successful
        break
      }, error = function(cond) {
        # Pause for 30 seconds before next try
        Sys.sleep(30)
        tries <<- tries + 1
        # If max number of tries has been reached, stop and return error
        if (tries > 5) {
          stop("Failed to retrieve data after multiple attempts")
        }
      })
    }
    
    # Pause for 10 second before making the next request
    Sys.sleep(10)
  }
  
  return(box_office_data)
}
