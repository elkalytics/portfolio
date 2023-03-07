#' Search for movie data on IMDb
#'
#' This function loops through a list of movie titles, searches for their data on IMDb using the imdbapi package, and combines the results into a single data frame.
#'
#' @param api_key Your IMDb API key. Get one at https://imdb-api.com/.
#' @param movies A vector of movie titles to search for on IMDb.
#'
#' @return A data frame containing movie data for all the movies searched.
#'
#' @import imdbapi
#'
#' @examples
#' api_key <- "YOUR-KEY"
#' movies <- c("The Godfather", "The Shawshank Redemption", "The Dark Knight")
#' all_results <- imdb_title_search(api_key, movies)
#' print(all_results)
#' 
#' # Combine the list of data frames into a single data frame
#' all_results <- do.call(rbind, movie_data_list)
#' 
#' # Remove duplicates based on imdbID
#' all_results <- all_results[!duplicated(all_results$imdbID), ]
#'
#' @export
library(imdbapi)
# Loop through the movies and retrieve their data
movie_data_list <- list()
for (i in seq(1, length(movies), 10)) {
  # Retrieve up to 10 movies at a time
  movie_subset <- movies[i:min(i+9, length(movies))]
  
  # Retrieve the data for the movies
  movie_data <- lapply(movie_subset, function(movie) find_by_title(movie, api_key = api_key))
  
  # Combine the data for the movies into a single data frame
  movie_df <- do.call(rbind, movie_data)
  
  # Add the data frame to the list of data frames
  movie_data_list[[length(movie_data_list)+1]] <- movie_df
}