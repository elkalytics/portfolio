#' Process Text
#'
#' This function takes a character vector of text and processes it into a tokenized
#' corpus with optional removal of stopwords and creation of n-grams.
#'
#' @param text A character vector of text
#' @param remove_stopwords A logical indicating whether to remove stopwords (default = TRUE)
#' @param ngrams An integer indicating the size of n-grams to create (default = 1)
#' @param output A string indicating the desired output format: "list" for a list of token vectors,
#'   or "character" for a character vector of tokens (default = "list")
#'
#' @return A list of token vectors, or a character vector of tokens if output = "character"
#'
#' @examples
#' raw_text <- "This is some raw text. It contains punctuation, digits, and stopwords. We will process it into a tokenized corpus."
#' tokens <- process_text(raw_text, remove_stopwords = TRUE, ngrams = 1)
#' tokens
#'
#' @importFrom stringi stri_trans_general
#' @importFrom quanteda stopwords tokens_ngrams
#' @export
# Load packages
library(stringi)
library(quanteda)
# Save function
process_text <- function(text, remove_stopwords = TRUE, ngrams = 1, output = "list") {
  # Convert text to lowercase and remove diacritical marks
  text <- stri_trans_general(text, "Latin-ASCII")
  
  # Split text into tokens
  tokens <- strsplit(text, "\\s+")
  
  # Remove stopwords if remove_stopwords is true
  if (remove_stopwords) {
    tokens <- lapply(tokens, function(x) x[!x %in% stopwords("english")])
  }
  
  # Simplify the tokenized corpus to a character vector
  tokens <- sapply(tokens, function(x) paste(x, collapse = " "))
  
  # Create n-grams if ngrams > 1
  if (ngrams > 1) {
    tokens <- tokens_ngrams(tokens, n = ngrams, concatenator = "_")
  }
  
  # Convert the tokenized corpus to the desired output format
  if (output == "list") {
    tokens <- lapply(tokens, strsplit, "_")
  } else if (output == "character") {
    tokens <- unlist(tokens)
  }
  
  # Return the tokenized corpus
  return(tokens)
}