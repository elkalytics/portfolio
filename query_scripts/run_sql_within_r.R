
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


