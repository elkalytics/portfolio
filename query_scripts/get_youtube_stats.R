#' Get like and view counts from a YouTube URL
#'
#' This function retrieves the number of views and likes for a YouTube video from its URL.
#' The output is saved to a data frame with the video URL, ID, date, views, and likes.
#'
#' @param url The URL of the YouTube video to retrieve stats from.
#'
#' @return A data frame with the video URL, ID, date, views, and likes.
#'
#' @import httr
#' @import magrittr
#' @import stringr
#'
#' @examples
#' stats <- get_youtube_stats("https://www.youtube.com/watch?v=dQw4w9WgXcQ")
#' print(stats)
# Save function
get_youtube_stats <- function(url) {
  require(httr)
  require(magrittr)
  require(stringr)
  
  # Get the video ID from the URL
  video_id <- str_extract(url, "(?<=v=)[^&\\n]+") %>% unique()
  
  # Get the HTML content of the video page
  res <- GET(url)
  html <- content(res, as = "text")
  
  # Extract the number of views and likes using regular expressions
  views_match <- "([\\d,]+)\\s+views"
  likes_match <- "([\\d,]+)\\s+likes"
  views <- str_match(html, views_match)[, 2] %>% gsub(",", "", .) %>% as.integer()
  likes <- str_match(html, likes_match)[, 2] %>% gsub(",", "", .) %>% as.integer()
  
  # Get the system date
  date <- Sys.Date()
  
  # Return a data frame with the video URL, ID, date, views, and likes
  data.frame(url = url, id = video_id, date = date, views = views, likes = likes)
}