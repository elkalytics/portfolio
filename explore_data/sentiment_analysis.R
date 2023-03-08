#' Perform sentiment analysis on a text data frame
#'
#' This function takes a data frame with text data and performs sentiment analysis
#' using the Syuzhet package. It returns a list with the overall sentiment score,
#' the overall sentiment label (positive, neutral, or negative), and the sentiment
#' score and label for each group (if a grouping variable is provided).
#'
#' @param df A data frame with text data.
#' @param text_column The name of the column with the text data.
#' @param grouping_var The name of the column to group the sentiment analysis by (optional).
#'
#' @return A list with the sentiment analysis results.
#' @keywords sentiment analysis, text mining
#'
#' @importFrom tidytext unnest_tokens
#' @importFrom syuzhet get_sentiment
#' @importFrom dplyr enquo group_by summarize mutate
#'
#' @examples
#' # Create a data frame with some text data
#' df <- data.frame(
#'   group = c("A", "B", "B", "C"),
#'   text = c("I love going to the beach.", 
#'            "I hate getting up early.", 
#'            "I feel neutral about this movie.", 
#'            "I'm not sure how I feel about this book.")
#' )
#'
#' # Perform sentiment analysis on the text data frame
#' sentiment_analysis(df, text_column = "text")
#' sentiment_analysis(df, text_column = "text", grouping_var = group)
#'
#' @export
sentiment_analysis <- function(df, text_column, grouping_var = NULL) {
  # Capture the grouping variable expression
  grouping_var <- enquo(grouping_var)
  
  # Create a document-term matrix
  df_tidy <- df %>%
    unnest_tokens(word, {{ text_column }})
  
  # Compute the sentiment score for each text and grouping variable
  if (length({{ grouping_var }}) > 0) {
    df_sentiment <- df_tidy %>%
      group_by(!!grouping_var) %>%
      summarize(sentiment_score = sum(get_sentiment(word)),
                .groups = "drop") %>%
      mutate(sentiment = ifelse(sentiment_score > 0, "Positive", 
                                ifelse(sentiment_score == 0, "Neutral", "Negative")))
  } else {
    df_sentiment <- df_tidy %>%
      summarize(sentiment_score = sum(get_sentiment(word))) %>%
      mutate(sentiment = ifelse(sentiment_score > 0, "Positive", 
                                ifelse(sentiment_score == 0, "Neutral", "Negative")))
  }
  
  # Compute the overall sentiment score
  overall_sentiment <- sum(df_sentiment$sentiment_score)
  
  # Compute the overall sentiment
  overall_sentiment_label <- ifelse(overall_sentiment > 0, "Positive", 
                                    ifelse(overall_sentiment == 0, "Neutral", "Negative"))
  
  # Return a list with the sentiment analysis results
  return(list(
    overall_sentiment = overall_sentiment,
    overall_sentiment_label = overall_sentiment_label,
    by_group_sentiment = df_sentiment
  ))
}