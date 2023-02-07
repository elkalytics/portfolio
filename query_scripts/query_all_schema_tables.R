
library(RODBC)

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


