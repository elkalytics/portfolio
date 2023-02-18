# Set the working directory to the directory where the RDS files are located
setwd("path/to/data/directory")

# Load the function
import_rds_files <- function(directory_path) {
  # Get a list of all the RDS files in the directory
  rds_files <- list.files(directory_path, pattern = "\\.rds$", full.names = TRUE)
  
  # Import each RDS file into the global environment
  for (file_path in rds_files) {
    obj_name <- sub("\\.rds$", "", basename(file_path))
    assign(obj_name, readRDS(file_path))
  }
}

# Call the function to import all the RDS files in the "data" directory
import_rds_files(".")

# Check that the objects were imported
ls()
