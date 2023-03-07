#' Scrape box office table for one date
#'
#' This function scrapes the box office table for a given date from the-numbers.com
#' using rvest package.
#'
#' @param date A character string representing the date in "YYYY/MM/DD" format
#' @return A data frame containing the box office table for the given date
#' @examples
#' scrape_box_office("2023/02/08")
#'
#' @import rvest
#' @export
# Load package
library(rvest)
scrape_box_office <- function(date) {
  # Construct URL for the given date
  url <- paste0("https://www.the-numbers.com/box-office-chart/daily/", date)
  
  # Scrape the table using CSS selectors
  webpage <- read_html(url)
  table <- html_table(html_nodes(webpage, "table")[2], fill = TRUE)
  
  return(table)
}