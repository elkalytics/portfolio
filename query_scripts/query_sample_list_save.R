#' Get a sample from every table in a PostgreSQL schema
#'
#' This function retrieves a sample of data from every table in a PostgreSQL schema and
#' saves the results in a list.
#'
#' @param conn a connection object created using \code{dbConnect()} from the \code{RPostgreSQL} package.
#'
#' @return a list containing a sample of data from every table in the schema.
#'
#' @examples
#' \dontrun{
#' conn <- dbConnect(
#'   dbDriver("PostgreSQL"),
#'   host="your_host",
#'   dbname="your_database",
#'   user="your_user",
#'   password="your_password"
#' )
#' samples <- sample_schema(conn)
#' dbDisconnect(conn)
#' }
#'
#' @import RPostgreSQL
#' @importFrom stats CEIL
#' @importFrom dplyr ROW_NUMBER
#' @importFrom dbplyr random
#'
#' @export
# Load package
library(RPostgreSQL)
sample_schema <- function(conn) {
  tables <- get_tables(conn)
  samples <- list()
  for (table in tables$table_name) {
    samples[[table]] <- sample_table(conn, table)
  }
  samples
}
#' Retrieve a list of tables in a PostgreSQL schema
#'
#' This function queries a PostgreSQL database and returns a data frame containing the names of all
#' tables in the specified schema.
#'
#' @param conn a connection object created using \code{dbConnect()} from the \code{RPostgreSQL} package.
#'
#' @return a data frame containing the names of all tables in the specified schema.
#'
#' @import RPostgreSQL
#'
#' @examples
#' \dontrun{
#' conn <- dbConnect(
#'   dbDriver("PostgreSQL"),
#'   host="your_host",
#'   dbname="your_database",
#'   user="your_user",
#'   password="your_password"
#' )
#' tables <- get_tables(conn)
#' dbDisconnect(conn)
#' }
#'
#' @export
# Save get table function
get_tables <- function(conn) {
  dbGetQuery(conn, "SELECT table_name
                   FROM information_schema.tables
                   WHERE table_schema = 'public'
                   AND table_type = 'BASE TABLE'")
}

#' Retrieve a sample of data from a PostgreSQL table
#'
#' This function retrieves a sample of data from a PostgreSQL table and returns it as a data frame.
#'
#' @param conn a connection object created using \code{dbConnect()} from the \code{RPostgreSQL} package.
#' @param table_name the name of the table to retrieve a sample from.
#'
#' @return a data frame containing a sample of data from the specified table.
#'
#' @import RPostgreSQL
#' @importFrom dplyr CEIL
#'
#' @examples
#' \dontrun{
#' conn <- dbConnect(
#'   dbDriver("PostgreSQL"),
#'   host="your_host",
#'   dbname="your_database",
#'   user="your_user",
#'   password="your_password"
#' )
#' sample_data <- sample_table(conn, "your_table_name")
#' dbDisconnect(conn)
#' }
#'
#' @export
sample_table <- function(conn, table_name) {
  query <- paste0("WITH cte AS (
                     SELECT *,
                            ROW_NUMBER() OVER (ORDER BY random()) AS rn
                     FROM ", table_name, "
                   )
                   SELECT *
                   FROM cte
                   WHERE rn <= CEIL(0.1 * (SELECT COUNT(*) FROM ", table_name, "))
                   LIMIT 100000;")
  dbGetQuery(conn, query)
}