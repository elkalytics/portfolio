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

SET @sql = N'SELECT ' + STUFF(@sql, 1, 1, '') + ' FROM table1 t1 JOIN table2 t2 ON t1.id = t2.id;';

EXEC (@sql);
