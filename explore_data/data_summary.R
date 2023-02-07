get_data_frame_summary <- function(df) {
  library(moments)
  
  result <- data.frame(matrix(ncol = 11, nrow = ncol(df)))
  colnames(result) <- c("Column Name", "Data Type", "Mean", "Median", "Mode",
                        "Sample SD", "Population SD", "Kurtosis", "Skewness", "Sum", "Absolute Sum")
  
  for (i in 1:ncol(df)) {
    result[i, 1] <- colnames(df)[i]
    column <- df[, i]
    column_type <- ifelse(is.numeric(column), "Numeric",
                          ifelse(is.factor(column), "Categorical",
                                 ifelse(is.character(column), "Character",
                                        ifelse(inherits(column, "Date"), "Date",
                                               ifelse(inherits(column, "POSIXt"), "Date/Time", "Unknown")))))
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


########################################################### How to use on 1 data frame

# Generate fake data set
set.seed(123)
df <- data.frame(numeric_col = rnorm(100),
                 categorical_col = sample(letters[1:5], 100, replace = TRUE),
                 character_col = sample(c("A", "B", "C"), 100, replace = TRUE),
                 date_col = as.Date("2021-01-01") + sample(365, 100, replace = TRUE),
                 time_col = as.POSIXct("2021-01-01") + sample(3600 * 24, 100, replace = TRUE),
                 numeric_col_na = c(rnorm(50), rep(NA, 50)))

# Use the function on the fake data set
result <- get_data_frame_summary(df)
result


########################################################### How to use on a list of data frames

# Generate fake data frames
set.seed(123)
df1 <- data.frame(numeric_col = rnorm(100),
                  categorical_col = sample(letters[1:5], 100, replace = TRUE),
                  character_col = sample(c("A", "B", "C"), 100, replace = TRUE),
                  date_col = as.Date("2021-01-01") + sample(365, 100, replace = TRUE),
                  time_col = as.POSIXct("2021-01-01") + sample(3600 * 24, 100, replace = TRUE),
                  numeric_col_na = c(rnorm(50), rep(NA, 50)))

df2 <- data.frame(numeric_col = rnorm(100),
                  categorical_col = sample(letters[1:5], 100, replace = TRUE),
                  character_col = sample(c("A", "B", "C"), 100, replace = TRUE),
                  date_col = as.Date("2021-01-01") + sample(365, 100, replace = TRUE),
                  time_col = as.POSIXct("2021-01-01") + sample(3600 * 24, 100, replace = TRUE),
                  numeric_col_na = c(rnorm(50), rep(NA, 50)))

df3 <- data.frame(numeric_col = rnorm(100),
                  categorical_col = sample(letters[1:5], 100, replace = TRUE),
                  character_col = sample(c("A", "B", "C"), 100, replace = TRUE),
                  date_col = as.Date("2021-01-01") + sample(365, 100, replace = TRUE),
                  time_col = as.POSIXct("2021-01-01") + sample(3600 * 24, 100, replace = TRUE),
                  numeric_col_na = c(rnorm(50), rep(NA, 50)))

df_list <- list(df1 = df1, df2 = df2, df3 = df3)

# Use the function on the fake data set
results_list <- lapply(df_list, get_data_frame_summary)