# Script designed to run all SQL scripts in a folder in sequential order by script name

# Load the RMySQL package
library(RMySQL)

# Connect to the database
con <- dbConnect(MySQL(), user="username", password="password", host="host", dbname="database")

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

# Disconnect from the database
dbDisconnect(con)
