-- SQL query most recent (based on system_date) the non-null value for each cell

-- Define a common table expression (CTE) named 'cte'
WITH cte AS (
  -- Select the column name, value, and system date for each row in 'table_name'
  SELECT 
    column_name,
    value,
    system_date,
    -- Use the ROW_NUMBER() function to assign a rank to each row within its partition (based on column_name),
    -- ordered by the system_date in descending order (i.e., most recent dates first)
    ROW_NUMBER() OVER (PARTITION BY column_name ORDER BY system_date DESC) AS rn
  FROM 
    table_name
)

-- Select the column name, most recent non-null date, and most recent non-null value for each column
SELECT 
  column_name,
  -- Use the MAX() function to select the most recent non-null system date
  MAX(CASE WHEN value IS NOT NULL THEN system_date END) AS latest_date,
  -- Use the MAX() function to select the most recent non-null value (for the row with rank 1)
  MAX(CASE WHEN rn = 1 AND value IS NOT NULL THEN value END) AS latest_value
FROM 
  cte
-- Group the results by column name
GROUP BY 
  column_name;