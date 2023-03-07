-- SQL query earliest non-null value for each cell based on a system_date field

-- Define a common table expression (CTE) named 'cte'
WITH cte AS (
  -- Select the column name, value, and system date for each row in 'table_name'
  SELECT 
    column_name,
    value,
    system_date,
    -- Use the ROW_NUMBER() function to assign a rank to each row within its partition (based on column_name),
    -- ordered by the system_date (i.e., earliest dates first)
    ROW_NUMBER() OVER (PARTITION BY column_name ORDER BY system_date) AS rn
  FROM 
    table_name
)

-- Select the column name, earliest non-null date, and earliest non-null value for each column
SELECT 
  column_name,
  -- Use the MIN() function to select the earliest non-null system date
  MIN(CASE WHEN value IS NOT NULL THEN system_date END) AS earliest_date,
  -- Use the MIN() function to select the earliest non-null value (for the row with rank 1)
  MIN(CASE WHEN rn = 1 AND value IS NOT NULL THEN value END) AS earliest_value
FROM 
  cte
-- Group the results by column name
GROUP BY 
  column_name;