#' Create an inverted index for a set of documents
#'
#' This function takes a character vector of documents and creates an inverted index
#' of unique words in the documents. The inverted index is a list where each element
#' is a word from the documents, and the value of that element is a vector of integers
#' indicating the frequency of that word in each document.
#'
#' @param documents A character vector of documents to create the inverted index for
#' @return A list representing the inverted index
#' @examples
#' documents <- c(
#'   "The quick brown fox jumps over the lazy dog",
#'   "The lazy dog slept all day",
#'   "The quick brown fox ate the lazy dog's bone",
#'   "The quick brown fox is a clever animal",
#'   "The lazy dog is a loyal companion"
#' )
#'
#' inverted_index <- create_inverted_index(documents)
#' print(inverted_index)
#' 
# Load package
library(tm)
# Save function
create_inverted_index <- function(documents) {
  # Tokenize the documents
  tokens <- strsplit(tolower(documents), "\\W+")
  
  # Remove stop words
  stop_words <- stopwords("english")
  tokens <- lapply(tokens, function(x) x[!x %in% stop_words])
  
  # Create a dictionary of unique words
  dictionary <- unique(unlist(tokens))
  
  # Create an empty inverted index
  inverted_index <- list()
  
  # Loop over the documents and update the inverted index
  for (i in seq_along(tokens)) {
    doc_tokens <- tokens[[i]]
    for (j in seq_along(doc_tokens)) {
      word <- doc_tokens[j]
      if (word %in% dictionary) {
        if (!word %in% names(inverted_index)) {
          inverted_index[[word]] <- integer(length(documents))
        }
        inverted_index[[word]][i] <- inverted_index[[word]][i] + 1
      }
    }
  }
  
  # Return the inverted index
  return(inverted_index)
}