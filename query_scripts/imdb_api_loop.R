# Function to loop imdb title search through list 10 at a time
# Results compile into "all_results"

library(imdbapi)

# Replace "your-api-key" with your actual API key
api_key <- "YOUR-KEY"

# Create a vector of movie titles
movies <- c("The Godfather", "The Shawshank Redemption", "The Dark Knight", "The Lord of the Rings: The Fellowship of the Ring", "Forrest Gump",
            "Inception", "The Matrix", "Pulp Fiction", "Fight Club", "The Silence of the Lambs",
            "The Usual Suspects", "Goodfellas", "Se7en", "The Green Mile", "The Prestige",
            "The Departed", "Gladiator", "The Lion King", "Jurassic Park", "Star Wars: Episode IV - A New Hope",
            "E.T. the Extra-Terrestrial", "Back to the Future", "Indiana Jones and the Raiders of the Lost Ark", "Jaws",
            "The Terminator", "Alien", "Blade Runner", "The Shining", "A Clockwork Orange")

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

# Combine the list of data frames into a single data frame
all_results <- do.call(rbind, movie_data_list)

# Remove duplicates based on imdbID
all_results <- all_results[!duplicated(all_results$imdbID), ]

