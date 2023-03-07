#' Query a PostgreSQL database for a sample of 10 rows from each table in the public schema
#'
#' This function connects to a PostgreSQL database, queries a random sample of 10 rows from every table in the "public" schema, and returns the results as a list of data frames, with each data frame representing a single table.
#'
#' @param dbname The name of the database to connect to.
#' @param host The hostname or IP address of the server hosting the database.
#' @param port The port number to use for the database connection.
#' @param user The username to use for the database connection.
#' @param password The password to use for the database connection.
#'
#' @return A list of data frames, with each data frame representing a single table in the "public" schema of the database and containing a random sample of 10 rows from that table.
#'
#' @import RPostgreSQL
#'
#' @examples
#' con <- dbConnect(PostgreSQL(),
#'                  dbname = "your_database_name",
#'                  host = "your_host",
#'                  port = "your_port",
#'                  user = "your_username",
#'                  password = "your_password")
#' df_list <- query_postgres_sample(con)
#' print(df_list)
#' dbDisconnect(con)
#'
#' @export
# Load library
library(RPostgreSQL)
# Retrieve a random sample of 10 rows from every table in the public schema
df_list <- lapply(dbListTables(con, schema = "public"), function(table_name) {
  sample_data <- dbGetQuery(con, paste("SELECT * FROM ", table_name, " TABLESAMPLE SYSTEM_ROWS(10)"))
  
  # Assign the column names to the sample data frame
  colnames(sample_data) <- dbGetQuery(con, paste("SELECT column_name FROM information_schema.columns WHERE table_name='", table_name, "'"))$column_name
  
  # Return the sample data frame with the table name as the key
  setNames(list(sample_data), table_name)
})