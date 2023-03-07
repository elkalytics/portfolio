#' Query tables from a SQL Server database schema
#'
#' This code connects to a SQL Server database using ODBC, queries all table names in the specified schema, and returns a list of tables with their corresponding data.
#'
#' @param db_connection The connection string for the SQL Server database.
#' @param schema_name The name of the schema to query tables from.
#'
#' @return A list of tables with their corresponding data.
#'
#' @import RODBC
#'
#' @examples
#' db_connection <- "DRIVER={SQL Server}; SERVER=<server_name>; DATABASE=<database_name>; UID=<username>; PWD=<password>"
#' schema_name <- "my_schema"
#' tables_list <- query_tables_from_schema(db_connection, schema_name)
#' print(tables_list)
#'
db_connection <- "DRIVER={SQL Server}; SERVER=<server_name>; DATABASE=<database_name>; UID=<username>; PWD=<password>"
schema_name <- "my_schema"
tables_list <- query_tables_from_schema(db_connection, schema_name)