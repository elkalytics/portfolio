# Function to get like and view counts from youtube URL
# Saves output to a data frame with URL, video ID, system date, views, and likes

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

## Use function
# stats <- get_youtube_stats("https://www.youtube.com/watch?v=dQw4w9WgXcQ")

## View results
# print(stats)
