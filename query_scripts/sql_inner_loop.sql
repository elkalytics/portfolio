-- SQL loop inner join for tables ending with "_final" which contain "index_var"

-- Declare variables for the table name and SQL query
DECLARE @table_name VARCHAR(100)
DECLARE @sql_query VARCHAR(MAX)
DECLARE @index_var_found BIT

-- Loop through all tables in the database that end with "_final"
DECLARE table_cursor CURSOR FOR
SELECT TABLE_NAME
FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_NAME LIKE '%_final'

OPEN table_cursor
FETCH NEXT FROM table_cursor INTO @table_name

WHILE @@FETCH_STATUS = 0
BEGIN
    -- Set the flag indicating whether the current table contains the 'index_var' column
    SET @index_var_found = 0
    
    -- Check if the current table contains the 'index_var' column
    SELECT @index_var_found = 1
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_NAME = @table_name
        AND COLUMN_NAME = 'index_var'

    -- If the current table contains the 'index_var' column, join it to the current result set
    IF @index_var_found = 1
    BEGIN
        -- Construct the inner join statement based on the current table
        IF @sql_query IS NULL
        BEGIN
            -- If this is the first table, simply select all columns from the table
            SET @sql_query = 'SELECT * FROM ' + @table_name
        END
        ELSE
        BEGIN
            -- If this is not the first table, join it to the previous result set using the 'index_var' column
            SET @sql_query = @sql_query + ' INNER JOIN ' + @table_name + ' ON ' + @table_name + '.index_var = ' + REPLACE(@table_name, '_final', '') + '.index_var'
        END
    END

    -- Fetch the next table
    FETCH NEXT FROM table_cursor INTO @table_name
END

-- Close and deallocate the cursor
CLOSE table_cursor
DEALLOCATE table_cursor

-- Execute the final query, which will join all tables that contain the 'index_var' column
EXEC(@sql_query)