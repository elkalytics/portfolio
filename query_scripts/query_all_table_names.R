#' Query data from tables in a SQL Server database folder
#'
#' This code connects to a SQL Server database using ODBC, retrieves a list of table names in the specified folder, queries the data from each table, and stores it in a list of data frames with the table name as the list name.
#'
#' @import RODBC
#'
#' @examples
#' conn <- odbcDriverConnect("Driver={SQL Server};Server=<server_name>;Database=<database_name>;Trusted_Connection=yes")
#' tables <- sqlTables(conn, tableType = "TABLE", schema = "<folder_name>")
#' table_names <- tables$TABLE_NAME
#' results_list <- list()
#' for (table in table_names) {
#'   query <- paste0("SELECT * FROM [", "<folder_name>", "].[", table, "]")
#'   results_list[[table]] <- sqlFetch(conn, query)
#' }
#' odbcClose(conn)
#' print(results_list)
#'
# Load library
library(RODBC)

# Connect to SQL Server database
conn <- odbcDriverConnect("Driver={SQL Server};Server=<server_name>;Database=<database_name>;Trusted_Connection=yes")

# Get a list of all table names in the specific folder
tables <- sqlTables(conn, tableType = "TABLE", schema = "<folder_name>")
table_names <- tables$TABLE_NAME

# Query data from each table and store it in a list
results_list <- list()
for (table in table_names) {
  query <- paste0("SELECT * FROM [", "<folder_name>", "].[", table, "]")
  results_list[[table]] <- sqlFetch(conn, query)
}

# Close the database connection
odbcClose(conn)