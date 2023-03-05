#' Summarize data frame statistics
#' 
#' This function calculates summary statistics for a data frame, including
#' data type, mean, median, mode, standard deviation, kurtosis, skewness, and 
#' sum, as well as the minimum and maximum values, number of NAs, and percentage
#' of non-null responses. It can also group the summary statistics by a specified
#' variable.
#' 
#' This function is meant to replace the pair of functions in the 'data_summary' script.
#' The user can get both statistics by group (if they select a grouping variable), as well
#' as statistics for the entire table.
#' 
#' @param df a data frame
#' @param group a variable to group the data frame by (optional)
#' @return a data frame or a list of data frames
#' @importFrom moments kurtosis skewness
#' @export
get_data_frame_summary <- function(df, group = NULL) {
  library(moments)
  
  if (is.null(group)) {
    result <- data.frame(matrix(ncol = 11, nrow = ncol(df)))
    colnames(result) <- c("Column Name", "Data Type", "Mean", "Median", "Mode",
                          "Sample SD", "Population SD", "Kurtosis", "Skewness", "Sum", "Absolute Sum")
    
    for (i in 1:ncol(df)) {
      column <- df[, i]
      column_type <- ifelse(is.numeric(column), "Numeric",
                            ifelse(is.factor(column), "Categorical",
                                   ifelse(is.character(column), "Character",
                                          ifelse(inherits(column, "Date"), "Date",
                                                 ifelse(inherits(column, "POSIXt"), "Date/Time", "Unknown")))))
      result[i, 1] <- colnames(df)[i]
      result[i, 2] <- column_type
      
      if (column_type == "Numeric") {
        result[i, 3] <- mean(column, na.rm = TRUE)
        result[i, 4] <- median(column, na.rm = TRUE)
        mode_value <- as.numeric(names(sort(-table(column), decreasing = TRUE)))[1]
        result[i, 5] <- mode_value
        result[i, 6] <- sd(column, na.rm = TRUE) / sqrt(nrow(df) - 1)
        result[i, 7] <- sd(column, na.rm = TRUE)
        result[i, 8] <- kurtosis(column, na.rm = TRUE)
        result[i, 9] <- skewness(column, na.rm = TRUE)
        result[i, 10] <- sum(column, na.rm = TRUE)
        result[i, 11] <- sum(abs(column), na.rm = TRUE)
      }
    }
    
    result <- cbind(result, min = sapply(df, function(x) 
      if (inherits(x, "Date") || inherits(x, "POSIXt")) {
        format(min(x, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
      } else {
        min(x, na.rm = TRUE)
      }),
      max = sapply(df, function(x) 
        if (inherits(x, "Date") || inherits(x, "POSIXt")) {
          format(max(x, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
        } else {
          max(x, na.rm = TRUE)
        }),
      n_na = colSums(is.na(df)),
      perc_non_null = round(100 * (1 - colSums(is.na(df)) / nrow(df)), 2))
    colnames(result)[12:15] <- c("Min", "Max", "Num NAs", "% Non-Null Responses")
    return(result)
  }
  
  result_list <- list()
  
  levels <- unique(df[, group])
  for (level in levels) {
    level_df <- df[df[, group] == level, ]
    result <- data.frame(matrix(ncol = 11, nrow = ncol(level_df)))
    colnames(result) <- c("Column Name", "Data Type", "Mean", "Median", "Mode",
                          "Sample SD", "Population SD", "Kurtosis", "Skewness", "Sum", "Absolute Sum")
    
    for (i in 1:ncol(level_df)) {
      column <- level_df[, i]
      column_type <- ifelse(is.numeric(column), "Numeric",
                            ifelse(is.factor(column), "Categorical",
                                   ifelse(is.character(column), "Character",
                                          ifelse(inherits(column, "Date"), "Date",
                                                 ifelse(inherits(column, "POSIXt"), "Date/Time", "Unknown")))))
      result[i, 1] <- colnames(level_df)[i]
      result[i, 2] <- column_type
      
      if (column_type == "Numeric") {
        result[i, 3] <- mean(column, na.rm = TRUE)
        result[i, 4] <- median(column, na.rm = TRUE)
        mode_value <- as.numeric(names(sort(-table(column), decreasing = TRUE)))[1]
        result[i, 5] <- mode_value
        result[i, 6] <- sd(column, na.rm = TRUE) / sqrt(nrow(level_df) - 1)
        result[i, 7] <- sd(column, na.rm = TRUE)
        result[i, 8] <- kurtosis(column, na.rm = TRUE)
        result[i, 9] <- skewness(column, na.rm = TRUE)
        result[i, 10] <- sum(column, na.rm = TRUE)
        result[i, 11] <- sum(abs(column), na.rm = TRUE)
      }
    }
    
    result <- cbind(result, min = sapply(level_df, function(x) 
      if (inherits(x, "Date") || inherits(x, "POSIXt")) {
        format(min(x, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
      } else {
        min(x, na.rm = TRUE)
      }),
      max = sapply(level_df, function(x) 
        if (inherits(x, "Date") || inherits(x, "POSIXt")) {
          format(max(x, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
        } else {
          max(x, na.rm = TRUE)
        }),
      n_na = colSums(is.na(level_df)),
      perc_non_null = round(100 * (1 - colSums(is.na(level_df)) / nrow(level_df)), 2))
    colnames(result)[12:15] <- c("Min", "Max", "Num NAs", "% Non-Null Responses")
    result_list[[as.character(level)]] <- result
  }
  
  return(result_list)
}