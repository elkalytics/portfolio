#' Converts numeric fields into categorical variables
#'
#' This function takes a data frame as input and converts all numeric fields
#' into categorical variables by creating 13 new variables for each one.
#' The 13 new variables are created using different methods like median split,
#' mean split, interquartile range split, proportional groups of 2, 3, 4, and 5,
#' top 25%, bottom 25%, top or bottom 25%, middle 50%, and z-score above or
#' below a certain threshold.
#'
#' @param df A data frame containing numeric variables.
#'
#' @return A data frame with 13 new variables for each numeric variable in the
#' original data frame.
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100, mean = 5, sd = 2),
#'   z = runif(100, min = 0, max = 10)
#' )
#' categorical_vars(df) -> results
#' head(results)
#'
#' @export
# Save function
categorical_vars <- function(df) {
  # identify numeric variables
  num_cols <- sapply(df, is.numeric)
  num_names <- names(df)[num_cols]
  
  # loop through numeric variables and create 13 new variables for each one
  for (name in num_names) {
    new_names <- paste0(name, "_", 1:13)
    
    # median split
    median_split <- median(df[[name]]) + runif(1, min = 0, max = 0.000001)
    median_split_cat <- as.factor(ifelse(df[[name]] < median_split, 0, 1))
    
    # mean split
    mean_split <- mean(df[[name]]) + runif(1, min = 0, max = 0.000001)
    mean_split_cat <- as.factor(ifelse(df[[name]] < mean_split, 0, 1))
    
    # interquartile range split
    qcut <- cut(df[[name]], quantile(df[[name]], c(0, 0.25, 0.5, 0.75, 1)), labels = c(1, 2, 3, 4))
    
    # proportional groups of 2, 3, 4, and 5
    prop2 <- cut(df[[name]], breaks = quantile(df[[name]], c(0, 0.5, 1)), labels = c(0, 1))
    prop3 <- cut(df[[name]], breaks = quantile(df[[name]], c(0, 1/3, 2/3, 1)), labels = c(1, 2, 3))
    prop4 <- cut(df[[name]], breaks = quantile(df[[name]], c(0, 0.25, 0.5, 0.75, 1)), labels = c(1, 2, 3, 4))
    prop5 <- cut(df[[name]], breaks = quantile(df[[name]], c(0, 0.2, 0.4, 0.6, 0.8, 1)), labels = c(1, 2, 3, 4, 5))
    
    # top 25%, bottom 25%, top or bottom 25%, and middle 50%
    top25 <- ifelse(df[[name]] >= quantile(df[[name]], 0.75), 1, 0)
    bot25 <- ifelse(df[[name]] <= quantile(df[[name]], 0.25), 1, 0)
    topbot25 <- ifelse(df[[name]] >= quantile(df[[name]], 0.75) | df[[name]] <= quantile(df[[name]], 0.25), 1, 0)
    mid50 <- ifelse(df[[name]] > quantile(df[[name]], 0.25) & df[[name]] < quantile(df[[name]], 0.75), 1, 0)
    
    # z-score above or below a certain threshold
    zscore <- scale(df[[name]])
    above196 <- ifelse(zscore > 1.96, 1, 0)
    belowm196 <- ifelse(zscore < -1.96, 1, 0)
    above0674 <- ifelse(zscore > 0.674, 1, 0)
    belowm0674 <- ifelse(zscore < -0.674, 1, 0)
    
    # add new variables to data frame
    df[, new_names[1]] <- median_split_cat
    df[, new_names[2]] <- mean_split_cat
    df[, new_names[3]] <- qcut
    df[, new_names[4]] <- prop2
    df[, new_names[5]] <- prop3
    df[, new_names[6]] <- prop4
    df[, new_names[7]] <- prop5
    df[, new_names[8]] <- top25
    df[, new_names[9]] <- bot25
    df[, new_names[10]] <- topbot25
    df[, new_names[11]] <- mid50
    df[, new_names[12]] <- above196 + belowm196
    df[, new_names[13]] <- above0674 + belowm0674
  }
  
  return(df)
}