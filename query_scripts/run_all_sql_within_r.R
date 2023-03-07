#' Execute SQL scripts in sequential order
#'
#' This script executes all SQL scripts in a folder in sequential order by script name.
#' 
#' @param user A character string specifying the database username.
#' @param password A character string specifying the database password.
#' @param host A character string specifying the database host.
#' @param dbname A character string specifying the database name.
#' @param script_dir A character string specifying the directory containing the SQL scripts.
#' 
#' @return None
#' 
#' @examples
#' \dontrun{
#' # Connect to the database
#' con <- dbConnect(MySQL(), user="username", password="password", host="host", dbname="database")
#' 
#' # Execute SQL scripts in a directory
#' execute_sql_scripts("username", "password", "host", "database", "/path/to/sql/scripts/")
#' 
#' # Disconnect from the database
#' dbDisconnect(con)
#' }
#'
execute_sql_scripts <- function(user, password, host, dbname, script_dir) {
  
  # Load the RMySQL package
  library(RMySQL)
  
  # Connect to the database
  con <- dbConnect(MySQL(), user=user, password=password, host=host, dbname=dbname)
  
  # Get a list of all SQL scripts in the script directory
  sql_scripts <- list.files(path = script_dir, pattern = "\\.sql$", full.names = TRUE)
  
  # Sort the list of scripts in order
  sql_scripts <- sort(sql_scripts)
  
  # Loop through each script and run it
  for (script in sql_scripts) {
    # Read in the SQL script
    script_text <- readLines(script)
    
    # Execute the script
    dbExecute(con, paste(script_text, collapse="\n"))
  }
  
  # Disconnect from the database
  dbDisconnect(con)
}