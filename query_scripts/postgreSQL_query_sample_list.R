# R script to query sample from all tables

library(RPostgreSQL)

# Set up database connection
con <- dbConnect(PostgreSQL(),
                 dbname = "your_database_name",
                 host = "your_host",
                 port = "your_port",
                 user = "your_username",
                 password = "your_password")

# Retrieve a random sample of 10 rows from every table in the public schema
df_list <- lapply(dbListTables(con, schema = "public"), function(table_name) {
  sample_data <- dbGetQuery(con, paste("SELECT * FROM ", table_name, " TABLESAMPLE SYSTEM_ROWS(10)"))
  
  # Assign the column names to the sample data frame
  colnames(sample_data) <- dbGetQuery(con, paste("SELECT column_name FROM information_schema.columns WHERE table_name='", table_name, "'"))$column_name
  
  # Return the sample data frame with the table name as the key
  setNames(list(sample_data), table_name)
})

# Close database connection
dbDisconnect(con)

# Combine the list of data frames into a single data frame
df <- do.call(rbind, df_list)

# View the list of data frames
print(df_list)
