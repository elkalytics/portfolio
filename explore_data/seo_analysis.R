#' Analyze SEO metrics of a given text
#'
#' This function calculates various metrics related to search engine optimization (SEO) of a given text.
#'
#' @param data A character string containing the text to be analyzed
#' @param target_keyword A character string representing the target keyword for which the metrics are to be calculated
#'
#' @return A list containing the following metrics:
#' \item{total_words}{The total number of words in the text}
#' \item{keyword_freq}{The frequency of the target keyword in the text}
#' \item{keyword_density}{The density of the target keyword in the text}
#' \item{average_word_length}{The average length of a word in the text}
#' \item{unique_words}{The number of unique words in the text}
#' \item{proximity}{The average proximity of the target keyword to other keywords or phrases in the text}
#' \item{readability_score}{The Flesch-Kincaid Readability Score of the text}
#'
#' @examples
#' data <- "Search engine optimization (SEO) is the process of improving the visibility of a website or a web page in a search engine's unpaid results-often referred to as 'natural,' 'organic,' or 'earned' results. In general, the earlier (or higher ranked on the search results page), and more frequently a website appears in the search results list, the more visitors it will receive from the search engine's users. SEO may target different kinds of search, including image search, video search, academic search, news search, and industry-specific vertical search engines. SEO differs from local search engine optimization in that the latter is focused on optimizing a business' online presence so that its web pages will be displayed by search engines when a user enters a local search for its products or services. The former instead is more focused on national or international searches."
#' target_keyword <- "SEO"
#' result <- seo_analysis(data, target_keyword)
#' print(result)
#'
#' @export
seo_analysis <- function(data, target_keyword) {
  
  # Calculate the total number of words in the data
  total_words <- sum(nchar(unlist(strsplit(data, " "))))
  
  # Calculate the frequency of the target keyword
  keyword_freq <- sum(grepl(target_keyword, unlist(strsplit(data, " "))))
  
  # Calculate the keyword density
  keyword_density <- (keyword_freq / total_words) * 100
  
  # Calculate the average word length
  average_word_length <- mean(nchar(unlist(strsplit(data, " "))))
  
  # Calculate the number of unique words
  unique_words <- length(unique(unlist(strsplit(data, " "))))
  
  # Calculate the average proximity of the target keyword to other keywords or phrases in the text
  proximity <- mean(sapply(strsplit(data, " "), function(x) {
    if(target_keyword %in% x) {
      min(abs(match(target_keyword, x) - seq_along(x)))
    } else {
      Inf
    }
  }))
  
  # Calculate the Flesch-Kincaid Readability Score
  syllables <- function(word) {
    sum(sapply(strsplit(word, ""), function(x) x %in% c("a", "e", "i", "o", "u")))
  }
  sentences <- sapply(strsplit(data, "[\\.\\?!]"), length)
  words <- sapply(strsplit(data, " "), length)
  syllable_count <- sapply(strsplit(data, " "), function(x) sum(sapply(x, syllables)))
  avg_words_per_sentence <- mean(words / sentences)
  avg_syllables_per_word <- mean(syllable_count / words)
  readability_score <- 206.835 - 1.015 * avg_words_per_sentence - 84.6 * avg_syllables_per_word
  
  # Output the results
  result <- list(
    total_words = total_words,
    keyword_freq = keyword_freq,
    keyword_density = keyword_density,
    average_word_length = average_word_length,
    unique_words = unique_words,
    proximity = proximity,
    readability_score = readability_score
  )
  return(result)
}