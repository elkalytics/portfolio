
db_connection <- "DRIVER={SQL Server}; SERVER=<server_name>; DATABASE=<database_name>; UID=<username>; PWD=<password>"
schema_name <- "my_schema"
tables_list <- query_tables_from_schema(db_connection, schema_name)

