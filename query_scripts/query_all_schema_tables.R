#' Query tables from a schema in a SQL server database
#'
#' This function connects to a SQL server database using ODBC, queries all table names in the specified schema, and then queries the data for each table, storing it in a list of data frames with the table name as the list name.
#'
#' @param db_connection The connection string for the SQL server database.
#' @param schema_name The name of the schema to query tables from.
#'
#' @return A list of data frames, with each data frame containing the data from a single table in the specified schema.
#'
#' @import RODBC
#'
#' @examples
#' db_connection <- "myserver/mydb;UID=myuser;PWD=mypassword"
#' schema_name <- "dbo"
#' table_data_list <- query_tables_from_schema(db_connection, schema_name)
#' print(table_data_list)
#'
#' @export
# Load package
library(RODBC)
# Save function
query_tables_from_schema <- function(db_connection, schema_name) {
  # Open the connection to the SQL server
  conn <- odbcConnect(db_connection)
  
  # Query all table names in the specified schema
  table_names_query <- paste0("SELECT table_name FROM information_schema.tables WHERE table_schema = '", schema_name, "'")
  table_names <- as.data.frame(sqlQuery(conn, table_names_query))
  
  # Create a list to store the data from each table
  table_data_list <- list()
  
  # Loop through each table and query its data
  for (table_name in table_names$table_name) {
    table_data_query <- paste0("SELECT * FROM ", schema_name, ".", table_name)
    table_data <- as.data.frame(sqlQuery(conn, table_data_query))
    
    # Assign the data to the list using the table name as the list name
    table_data_list[[table_name]] <- table_data
  }
  
  # Close the connection to the SQL server
  odbcClose(conn)
  
  return(table_data_list)
}