# Scrape box office table for one date
library(rvest)

scrape_box_office <- function(date) {
  # Construct URL for the given date
  url <- paste0("https://www.the-numbers.com/box-office-chart/daily/", date)
  
  # Scrape the table using CSS selectors
  webpage <- read_html(url)
  table <- html_table(html_nodes(webpage, "table")[2], fill = TRUE)
  
  return(table)
}

## Example
# scrape_box_office("2023/02/08") -> box_office_totals