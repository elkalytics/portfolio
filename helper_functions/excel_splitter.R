# A script designed to take a workbook and split it into multiple sheets based on desired # of rows

library(readxl)
library(readr)

split_excel_to_csv <- function(excel_file, rows_per_csv=1000) {
  # Read the Excel workbook into a data frame
  df <- read_excel(excel_file)
  
  # Determine the number of CSV files needed
  num_csvs <- ceiling(nrow(df) / rows_per_csv)
  
  # Split the data frame into a list of data frames, each with the specified number of rows
  df_list <- split(df, rep(1:num_csvs, each=rows_per_csv, length.out=nrow(df)))
  
  # Write each data frame to a CSV file
  for (i in 1:num_csvs) {
    write_csv(df_list[[i]], paste0(excel_file, '_', format(Sys.Date(), '%Y%m%d'), '_', i, '.csv'))
  }
}



# Apply function to data at its path and it will save the output in the same path
split_excel_to_csv("C:/Users/JChas/OneDrive/Desktop/Output/examples.xlsx", 1000)


