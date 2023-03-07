-- Query min and max from table using a dynamic query
-- Avoid specifying all the names
-- Syntax for SQL server

-- Declare a variable for the dynamic SQL query
DECLARE @sql NVARCHAR(MAX);

-- Initialize the variable with an empty string
SET @sql = N'';

-- Select the minimum and maximum values for each column in the specified table,
-- using dynamic SQL to avoid specifying all column names explicitly
SELECT @sql = @sql + ',' + N'MIN(' + QUOTENAME(column_name) + ') AS ' + QUOTENAME(column_name + '_min') + 
                ',' + N'MAX(' + QUOTENAME(column_name) + ') AS ' + QUOTENAME(column_name + '_max')
FROM 
  information_schema.columns 
WHERE 
  table_name = 'table_name'

-- Construct the full dynamic SQL query string, using the table name and the
-- minimum and maximum values for each column
SET @sql = N'SELECT ' + STUFF(@sql, 1, 1, '') + ' FROM ' + QUOTENAME(table_name) + ';';

-- Execute the dynamic SQL query
EXEC (@sql);
