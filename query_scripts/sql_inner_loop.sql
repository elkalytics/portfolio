## SQL loop inner join for tables ending with "_final" which contain "index_var"

DECLARE @table_name VARCHAR(100)
DECLARE @sql_query VARCHAR(MAX)
DECLARE @index_var_found BIT

-- Loop through all tables in the database
DECLARE table_cursor CURSOR FOR
SELECT TABLE_NAME
FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_NAME LIKE '%_final'

OPEN table_cursor
FETCH NEXT FROM table_cursor INTO @table_name

WHILE @@FETCH_STATUS = 0
BEGIN
    SET @index_var_found = 0
    
    -- Check if the table contains the column 'index_var'
    SELECT @index_var_found = 1
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_NAME = @table_name
        AND COLUMN_NAME = 'index_var'

    -- If the table contains 'index_var', join it to the current result set
    IF @index_var_found = 1
    BEGIN
        IF @sql_query IS NULL
        BEGIN
            SET @sql_query = 'SELECT * FROM ' + @table_name
        END
        ELSE
        BEGIN
            SET @sql_query = @sql_query + ' INNER JOIN ' + @table_name + ' ON ' + @table_name + '.index_var = ' + REPLACE(@table_name, '_final', '') + '.index_var'
        END
    END

    FETCH NEXT FROM table_cursor INTO @table_name
END

CLOSE table_cursor
DEALLOCATE table_cursor

-- Execute the final query
EXEC(@sql_query)
