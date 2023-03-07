-- Identifies date problems

-- Declare variable to store the date column name
DECLARE @date_column_name NVARCHAR(100);

-- Assign the name of the date column to the variable
SET @date_column_name = (SELECT COLUMN_NAME
                         FROM INFORMATION_SCHEMA.COLUMNS
                         WHERE TABLE_NAME = 'table_name' -- Replace with the name of your table
                           AND DATA_TYPE = 'date');

-- Check if the table contains a date column
IF @date_column_name IS NOT NULL
BEGIN
  -- Cast the date column to date type and select all rows with invalid dates
  WITH cte AS (
    SELECT *, 
           CAST(@date_column_name AS DATE) AS casted_date
    FROM table_name -- Replace with the name of your table
  )
  SELECT *
  INTO date_problems -- Create a new table to store the rows with invalid dates
  FROM cte
  WHERE casted_date IS NULL OR casted_date != @date_column_name;

  -- Check if any invalid dates were found
  IF EXISTS (SELECT *
             FROM date_problems)
  BEGIN
    -- Print message if invalid dates were found
    PRINT 'Problems with date formatting or validity have been identified and saved to the "date_problems" table.';
  END
  ELSE
  BEGIN
    -- Print message if no invalid dates were found and drop the "date_problems" table
    PRINT 'No problems with date formatting or validity were found.';
    DROP TABLE date_problems;
  END;
END
ELSE
BEGIN
  -- Print message if no date column was found in the table
  PRINT 'No date columns were found in the table.';
END