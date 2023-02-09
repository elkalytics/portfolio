# SQL server descriptive query

DECLARE @table_name VARCHAR(255);
DECLARE @query VARCHAR(MAX);
DECLARE @results TABLE (
  table_name VARCHAR(255),
  column_name VARCHAR(255),
  mean FLOAT,
  median FLOAT,
  mode FLOAT,
  highest FLOAT,
  lowest FLOAT
);

DECLARE table_cursor CURSOR FOR
  SELECT table_name
  FROM information_schema.tables
  WHERE table_schema = 'your_schema_name';

OPEN table_cursor;

FETCH NEXT FROM table_cursor INTO @table_name;

WHILE @@FETCH_STATUS = 0
BEGIN
  SET @query = '
  SELECT ' + @table_name + '.column_name,
    AVG(CASE WHEN ISNUMERIC(' + @table_name + '.value) = 1 THEN ' + @table_name + '.value ELSE NULL END) AS mean,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ' + @table_name + '.value) AS median,
    MODE() WITHIN GROUP (ORDER BY ' + @table_name + '.value) AS mode,
    MAX(' + @table_name + '.value) AS highest,
    MIN(' + @table_name + '.value) AS lowest
  FROM ' + @table_name;

  INSERT INTO @results
  EXEC (@query);

  FETCH NEXT FROM table_cursor INTO @table_name;
END

CLOSE table_cursor;
DEALLOCATE table_cursor;

SELECT *
FROM @results;