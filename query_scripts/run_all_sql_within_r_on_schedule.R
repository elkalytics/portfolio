# Load the required packages
library(RMySQL)
library(cronR)

# Connect to the database
con <- dbConnect(MySQL(), user="username", password="password", host="host", dbname="database")

# Define a function to run the SQL scripts
run_sql_scripts <- function() {
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

# Schedule the task to run daily at 4 PM
cron_job <- cron_schedule(run_command = run_sql_scripts, hour = 16)
cronR::add_job(cron_job)

# Disconnect from the database
dbDisconnect(con)
