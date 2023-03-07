#' Import CSV and XLSX files into a list
#'
#' @return A list containing data frames imported from CSV and XLSX files
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom readr read_csv
#'
#' @examples 
#' files <- import_files_to_list()
#' str(files)
#'
#' @export 
# Load packages
library(readxl)
library(readr)
# Save function
import_files_to_list <- function() {
  filenames <- list.files(pattern = "(csv|xlsx)$")
  imported_files <- list()
  
  for (i in 1:length(filenames)) {
    filename <- filenames[i]
    name <- gsub("\\.(csv|xlsx)$", "", filename)
    name_exists <- name %in% names(imported_files)
    if (name_exists) {
      name_index <- 1
      while (name_exists) {
        name_index <- name_index + 1
        new_name <- paste0(name, "_", name_index)
        name_exists <- new_name %in% names(imported_files)
      }
      name <- new_name
    }
    
    if (grepl("csv$", filename)) {
      imported_files[[name]] <- read_csv(filename)
    } else {
      sheets <- readxl::excel_sheets(filename)
      if (length(sheets) > 1) {
        for (j in 1:length(sheets)) {
          sheet_name <- sheets[j]
          imported_files[[sheet_name]] <- readxl::read_excel(filename, sheet = sheet_name)
        }
      } else {
        imported_files[[name]] <- readxl::read_excel(filename)
      }
    }
  }
  
  return(imported_files)
}