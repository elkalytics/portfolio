
classify_columns <- function(df) {
  col_types <- rep("unknown", ncol(df))
  
  for (i in seq_along(df)) {
    col <- df[[i]]
    
    if (is.numeric(col)) {
      col_types[i] <- "numeric"
    } else if (all(col %in% c(0, 1))) {
      col_types[i] <- "binary"
    } else if (all(col %in% c(TRUE, FALSE))) {
      col_types[i] <- "logical"
    } else if (all(is.Date(col))) {
      col_types[i] <- "date"
    } else if (all(is.POSIXct(col))) {
      col_types[i] <- "timestamp"
    } else if (is.factor(col) && length(unique(col)) <= 20) {
      col_types[i] <- "categorical"
    } else {
      col_types[i] <- "unknown"
    }
  }
  
  col_types
}
# Create example data frame
df <- data.frame(
  numeric_col_1 = rnorm(100),
  binary_col_1 = sample(c(0, 1), 100, replace = TRUE),
  logical_col_1 = sample(c(TRUE, FALSE), 100, replace = TRUE),
  date_col_1 = seq(from = as.Date("2022-01-01"), to = as.Date("2022-01-100"), by = "day"),
  timestamp_col_1 = seq(from = as.POSIXct("2022-01-01 00:00:00"), to = as.POSIXct("2022-01-100 23:59:59"), by = "hour"),
  categorical_col_1 = sample(letters[1:20], 100, replace = TRUE),
  identifier_col_1 = paste0("ID", 1:100),
  numeric_col_2 = rnorm(100),
  binary_col_2 = sample(c(0, 1), 100, replace = TRUE),
  logical_col_2 = sample(c(TRUE, FALSE), 100, replace = TRUE),
  date_col_2 = seq(from = as.Date("2022-01-01"), to = as.Date("2022-01-100"), by = "day"),
  timestamp_col_2 = seq(from = as.POSIXct("2022-01-01 00:00:00"), to = as.POSIXct("2022-01-100 23:59:59"), by = "hour"),
  categorical_col_2 = sample(letters[1:20], 100, replace = TRUE),
  identifier_col_2 = paste0("ID", 1:100)
)

# Apply classify_columns function
classify_columns(df)
