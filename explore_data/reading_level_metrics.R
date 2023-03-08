#' Gunning Fog Index
#'
#' This function calculates the Gunning Fog Index.
#'
#' @param words The number of words in the text.
#' @param sentences The number of sentences in the text.
#' @param complex_words The number of complex words in the text.
#'
#' @return The Gunning Fog Index.
#'
#' @export
gunning_fog_index <- function(words, sentences, complex_words) {
  return(0.4 * ((words / sentences) + 100 * (complex_words / words)))
}

#' Coleman-Liau Index
#'
#' This function calculates the Coleman-Liau Index.
#'
#' @param characters The number of characters in the text.
#' @param words The number of words in the text.
#' @param sentences The number of sentences in the text.
#'
#' @return The Coleman-Liau Index.
#'
#' @export
coleman_liau_index <- function(characters, words, sentences) {
  return(0.0588 * (characters / words * 100) - 0.296 * (sentences / words * 100) - 15.8)
}

#' SMOG Index
#'
#' This function calculates the SMOG Index.
#'
#' @param poly_words The number of polysyllabic words in the text.
#' @param sentences The number of sentences in the text.
#'
#' @return The SMOG Index.
#'
#' @export
smog_index <- function(poly_words, sentences) {
  return(sqrt(30 * (poly_words / sentences)) + 3)
}

#' Reading Level Metrics
#'
#' This function calculates the reading level of a given text using multiple metrics.
#'
#' @param df A data frame.
#' @param column_name The name of the column in the data frame containing the text.
#'
#' @return A data frame with the reading level for each metric.
#'
#' @example 
#' 
#' # Example usage
#' data <- data.frame(text_column = c("This is some sample text.", "It contains multiple sentences."))
#' reading_levels <- reading_level_metrics(data, "text_column")
#' print(reading_levels)
#'
#' @export
reading_level_metrics <- function(df, column_name) {
  # Extract the text from the specified column of the data frame
  text <- as.character(df[[column_name]])
  
  # Split the text into words
  words <- unlist(strsplit(text, "\\W+"))
  
  # Count the number of words, sentences, and complex words
  num_words <- length(words)
  num_sentences <- length(gregexpr("[[:punct:]]+[[:space:]]+[[:upper:]]", text)[[1]]) + 1
  num_complex_words <- sum(sapply(words, function(w) {nchar(w) > 2 & nchar(gsub("[aeiouy]", "", w)) > 1}))
  
  # Calculate the reading level using multiple metrics
  fk_grade_level <- 0.39 * (num_words / num_sentences) + 11.8 * (sum(nchar(words)) / num_words) - 15.59
  gf_index <- gunning_fog_index(num_words, num_sentences, num_complex_words)
  cl_index <- coleman_liau_index(sum(nchar(words)), num_words, num_sentences)
  smog_index <- smog_index(num_complex_words, num_sentences)
  
  # Construct a data frame with the reading level for each metric
  df_metrics <- data.frame(
    Metric = c("Flesch-Kincaid Grade Level", "Gunning Fog Index", "Coleman-Liau Index", "SMOG Index"),
    Reading_Level = c(fk_grade_level, gf_index, cl_index, smog_index)
  )
  
  return(df_metrics)
}