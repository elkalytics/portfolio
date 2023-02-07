library(RMySQL)

# Connect to MySQL database
conn <- dbConnect(MySQL(), host = "<host_name>", username = "<username>", password = "<password>", dbname = "<database_name>")

# Get a list of all table names in the specific folder
tables <- dbGetQuery(conn, "SHOW TABLES FROM <database_name> LIKE '<folder_name>%'")
table_names <- tables[,1]

# Loop through each table and count the number of nulls for each column
for (table in table_names) {
  # Get a list of all columns in the table
  columns <- dbGetQuery(conn, paste0("SHOW COLUMNS FROM ", table))
  column_names <- columns[,1]
  
  # Loop through each column and count the number of nulls
  for (column in column_names) {
    query <- paste0("SELECT COUNT(*) FROM ", table, " WHERE ISNULL(`", column, "`)")
    nulls <- dbGetQuery(conn, query)
    print(paste0("The number of nulls in column ", column, " in table ", table, " is: ", as.numeric(nulls[1,1])))
  }
}

# Close the database connection
dbDisconnect(conn)
