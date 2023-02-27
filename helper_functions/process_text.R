# Function to process text
# Remove stop wards and add ngrams
# Saves as tokenized corpus

# Load packages
library(stringi)
library(quanteda)

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



## Save example raw text
# raw_text <- "This is some raw text. It contains punctuation, digits, and stopwords. We will process it into a tokenized corpus."

## Process text
# tokens <- process_text(raw_text, remove_stopwords = TRUE, ngrams = 1) # default behavior

## View results
# tokens
