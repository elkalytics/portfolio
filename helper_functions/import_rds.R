#' Import all RDS files in a directory
#'
#' This function takes the path to a directory as input and imports all the RDS files
#' in that directory into the global environment. The object names are derived from the
#' filenames of the RDS files.
#'
#' @param directory_path A character string representing the path to the directory
#' containing the RDS files
#'
#' @return NULL
#'
#' @importFrom base assign basename list.files sub
#' @importFrom utils readRDS
#' @export
#'
#' @examples
#' # Assume the current working directory contains a subdirectory called "data"
#' # containing several RDS files
#' import_rds_files("data")
# Save function
import_rds_files <- function(directory_path) {
  # Get a list of all the RDS files in the directory
  rds_files <- list.files(directory_path, pattern = "\\.rds$", full.names = TRUE)
  
  # Import each RDS file into the global environment
  for (file_path in rds_files) {
    obj_name <- sub("\\.rds$", "", basename(file_path))
    assign(obj_name, readRDS(file_path))
  }
}
