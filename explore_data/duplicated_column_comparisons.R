library(janitor)

count_cols_across_tables <- function(tables) {
  cleaned_tables <- lapply(tables, function(x) clean_names(x))
  col_counts <- lapply(cleaned_tables, function(x) colSums(!is.na(x)))
  col_data <- lapply(cleaned_tables, function(x) {
    lapply(colnames(x), function(col) {
      non_null_val <- x[!is.na(x[[col]]), col][1]
      list(col = col, first_non_null = non_null_val)
    })
  })
  
  col_data <- unlist(col_data, recursive = FALSE)
  col_data <- lapply(col_data, function(x) {
    x$tables <- names(cleaned_tables)[sapply(col_counts, function(y) y[x$col] > 0)]
    x
  })
  
  col_data <- do.call(rbind, col_data)
  col_data <- as.data.frame(col_data)
  
  col_data
}



# create two data.frames with different number of rows and columns
df1 <- data.frame(col1 = c(1, 2, 3),
                  col2 = c(4, 5, 6),
                  col3 = c(7, 8, 9))

df2 <- data.frame(col1 = c(5, 10),
                  col2 = c(3, 4),
                  col3 = c(5, 6),
                  col4 = c(7, 8))

df3 <- data.frame(col1 = c(5, 10),
                  col4 = c("A", "B"))

# create a list of data.frames
tables <- list(df1 = df1, df2 = df2, df3 = df3)

# call the function
col_data <- count_cols_across_tables(tables)

# display the output
col_data
