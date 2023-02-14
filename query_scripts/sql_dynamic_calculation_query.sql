-- Query min and max from table using a dynamic query
-- Avoid specifying all the names
-- Syntax for SQL server

DECLARE @sql NVARCHAR(MAX);
SET @sql = N'';

SELECT @sql = @sql + ',' + N'MIN(' + QUOTENAME(column_name) + ') AS ' + QUOTENAME(column_name + '_min') + 
                ',' + N'MAX(' + QUOTENAME(column_name) + ') AS ' + QUOTENAME(column_name + '_max')
FROM 
  information_schema.columns 
WHERE 
  table_name = 'table_name'

SET @sql = N'SELECT ' + STUFF(@sql, 1, 1, '') + ' FROM ' + QUOTENAME(table_name) + ';';

EXEC (@sql);
