#' Load the required packages
#' 
#' @import RMySQL
#' @importFrom cronR cron_schedule add_job
library(RMySQL)
library(cronR)

#' Connect to the database
#' 
#' @param user The database username
#' @param password The database password
#' @param host The database host
#' @param dbname The name of the database
#' @return A database connection object
#' @export
connect_to_database <- function(user, password, host, dbname) {
  dbConnect(MySQL(), user=user, password=password, host=host, dbname=dbname)
}

#' Define a function to run the SQL scripts
#' 
#' @param con A database connection object
#' @export
run_sql_scripts <- function(con) {
  # Get a list of all SQL scripts in the working directory
  sql_scripts <- list.files(pattern = "*.sql")
  
  # Sort the list of scripts in order
  sql_scripts <- sort(sql_scripts)
  
  # Loop through each script and run it
  for (script in sql_scripts) {
    # Read in the SQL script
    script_text <- readLines(script)
    
    # Execute the script
    dbExecute(con, paste(script_text, collapse="\n"))
  }
}

#' Schedule the task to run daily at a specified time
#' 
#' @param con A database connection object
#' @param hour The hour of the day to run the task (0-23)
#' @param minute The minute of the hour to run the task (0-59)
#' @param second The second of the minute to run the task (0-59)
#' @return A cron job object
#' @export
schedule_daily_task <- function(con, hour, minute = 0, second = 0) {
  cron_job <- cron_schedule(run_command = function() {run_sql_scripts(con)}, 
                            minute = minute, 
                            hour = hour, 
                            second = second)
  cronR::add_job(cron_job)
  return(cron_job)
}

#' Disconnect from the database
#' 
#' @param con A database connection object
#' @export
disconnect_from_database <- function(con) {
  dbDisconnect(con)
}