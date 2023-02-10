# Identifies date problems

DECLARE @date_column_name NVARCHAR(100);

SET @date_column_name = (SELECT COLUMN_NAME
                         FROM INFORMATION_SCHEMA.COLUMNS
                         WHERE TABLE_NAME = 'table_name'
                           AND DATA_TYPE = 'date');

IF @date_column_name IS NOT NULL
BEGIN
  WITH cte AS (
    SELECT *, 
           CAST(@date_column_name AS DATE) AS casted_date
    FROM table_name
  )
  SELECT *
  INTO date_problems
  FROM cte
  WHERE casted_date IS NULL OR casted_date != @date_column_name;

  IF EXISTS (SELECT *
             FROM date_problems)
  BEGIN
    PRINT 'Problems with date formatting or validity have been identified and saved to the "date_problems" table.';
  END
  ELSE
  BEGIN
    PRINT 'No problems with date formatting or validity were found.';
    DROP TABLE date_problems;
  END;
END
ELSE
BEGIN
  PRINT 'No date columns were found in the table.';
END
