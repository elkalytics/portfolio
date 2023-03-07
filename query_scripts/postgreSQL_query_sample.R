#' Query a PostgreSQL database for a sample of 10 rows from each table
#'
#' This function connects to a PostgreSQL database, queries a sample of 10 rows from each table in the "public" schema, and returns the results in a data frame.
#'
#' @param dbname The name of the database to connect to.
#' @param host The hostname or IP address of the server hosting the database.
#' @param port The port number to use for the database connection.
#' @param user The username to use for the database connection.
#' @param password The password to use for the database connection.
#'
#' @return A data frame containing a sample of 10 rows from each table in the "public" schema of the database.
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
#' sample_data <- query_postgres_sample(con)
#' print(sample_data)
#' dbDisconnect(con)
#'
#' @export
# Load library
library(RPostgreSQL)
# Query sample of 10 rows from each table
sample_data <- dbGetQuery(con, "
  SELECT * FROM (
    SELECT table_name, column_name FROM information_schema.columns
    WHERE table_schema = 'public'
    ORDER BY table_name, column_name
  ) AS cols
  JOIN (
    SELECT table_name, *
    FROM (
      SELECT table_name, ROW_NUMBER() OVER (ORDER BY RANDOM()) as rownum
      FROM information_schema.tables
      WHERE table_schema = 'public'
    ) AS rndm
    JOIN information_schema.tables t ON t.table_name = rndm.table_name
    WHERE rndm.rownum <= 10
  ) AS tabs ON cols.table_name = tabs.table_name
  ORDER BY tabs.table_name, cols.column_name")