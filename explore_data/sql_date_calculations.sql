-- Calculate distance between dates (fields ending with "_dt") which have the same name between the two data tables

DECLARE @sql NVARCHAR(MAX);
SET @sql = N'';

SELECT @sql = @sql + ', DATEDIFF(day, ' + QUOTENAME(t1.column_name) + ', ' + QUOTENAME(t2.column_name) + ') AS ' + QUOTENAME(t1.column_name + '_difference')
FROM 
  information_schema.columns t1
  JOIN information_schema.columns t2 
    ON t1.column_name = t2.column_name
WHERE 
  t1.table_name = 'table1' 
  AND t2.table_name = 'table2' 
  AND t1.column_name LIKE '%_dt';

-- @sql now contains a comma-separated list of expressions like: ', DATEDIFF(day, [date_col_1], [date_col_2]) AS [date_col_1_difference], ...'

SET @sql = N'SELECT ' + STUFF(@sql, 1, 1, '') + ' FROM table1 t1 JOIN table2 t2 ON t1.id = t2.id;';

-- The STUFF function removes the first comma in the list, and the resulting string is concatenated with the rest of the SELECT statement to form a complete dynamic SQL statement.

EXEC (@sql);
