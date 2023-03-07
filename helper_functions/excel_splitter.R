#' Split an Excel workbook into multiple CSV files
#'
#' This function takes an Excel workbook and splits it into multiple CSV files, each containing 
#' the specified number of rows. The output CSV files will be named according to the original 
#' Excel file name, the current date, and an index indicating which file it is (e.g. 
#' "examples_20220307_1.csv").
#'
#' @param excel_file A string specifying the path to the Excel workbook to split
#' @param rows_per_csv An integer specifying the number of rows to include in each output CSV file
#' 
#' @import readxl
#' @import readr
#'
#' @examples
#' # Split an example Excel workbook into CSV files containing 1000 rows each
#' split_excel_to_csv("path/examples.xlsx", 1000)
#'
#' @export
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