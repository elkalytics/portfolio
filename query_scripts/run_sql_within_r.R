#' Install and load RMySQL package, connect to MySQL database, execute SQL script and disconnect
#'
#' This script installs the RMySQL package if not already installed, connects to a MySQL database using the provided credentials, reads in an SQL script from a specified file path, executes the script and disconnects from the database.
#'
#' This script is for illustrative purposes only.
#' 
#' @param user MySQL username
#' @param password MySQL password
#' @param host MySQL host name or IP address
#' @param dbname MySQL database name
#' @param script_path File path of SQL script to be executed
#' @return No explicit return value, but the SQL script will be executed on the database
#'
#' @examples
#' \dontrun{
#' execute_script("my_user", "my_password", "localhost", "my_database", "path/to/script.sql")
#' }
#'
#' @import RMySQL
#' @importFrom utils readLines
#' @export
# First, install the RMySQL package
install.packages("RMySQL")

# Load the package
library(RMySQL)

# Connect to the database
con <- dbConnect(MySQL(), user="username", password="password", host="host", dbname="database")

# Read in the SQL script
script <- readLines("path/to/script.sql")

# Execute the script
dbExecute(con, paste(script, collapse="\n"))

# Disconnect from the database
dbDisconnect(con)