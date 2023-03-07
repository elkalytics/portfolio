-- SQL server descriptive query

-- Declare variables
DECLARE @table_name VARCHAR(255);
DECLARE @query VARCHAR(MAX);

-- Create a table to store results
DECLARE @results TABLE (
  table_name VARCHAR(255),
  column_name VARCHAR(255),
  mean FLOAT,
  median FLOAT,
  mode FLOAT,
  highest FLOAT,
  lowest FLOAT
);

-- Declare a cursor to loop through all the tables in the schema
DECLARE table_cursor CURSOR FOR
  SELECT table_name
  FROM information_schema.tables
  WHERE table_schema = 'your_schema_name';

-- Open the cursor
OPEN table_cursor;

-- Fetch the first table name
FETCH NEXT FROM table_cursor INTO @table_name;

-- Loop through all tables
WHILE @@FETCH_STATUS = 0
BEGIN
  -- Build the dynamic SQL query
  SET @query = '
  SELECT ' + @table_name + '.column_name,
    AVG(CASE WHEN ISNUMERIC(' + @table_name + '.value) = 1 THEN ' + @table_name + '.value ELSE NULL END) AS mean,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ' + @table_name + '.value) AS median,
    MODE() WITHIN GROUP (ORDER BY ' + @table_name + '.value) AS mode,
    MAX(' + @table_name + '.value) AS highest,
    MIN(' + @table_name + '.value) AS lowest
  FROM ' + @table_name;

  -- Execute the query and insert the results into the table
  INSERT INTO @results
  EXEC (@query);

  -- Fetch the next table name
  FETCH NEXT FROM table_cursor INTO @table_name;
END

-- Close and deallocate the cursor
CLOSE table_cursor;
DEALLOCATE table_cursor;

-- Select all columns from the results table
SELECT *
FROM @results;
