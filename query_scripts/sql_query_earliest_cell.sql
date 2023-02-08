# SQL query earliest non-null value for each cell based on a system_date field

WITH cte AS (
  SELECT 
    column_name,
    value,
    system_date,
    ROW_NUMBER() OVER (PARTITION BY column_name ORDER BY system_date) AS rn
  FROM 
    table_name
)
SELECT 
  column_name,
  MIN(CASE WHEN value IS NOT NULL THEN system_date END) AS earliest_date,
  MIN(CASE WHEN rn = 1 AND value IS NOT NULL THEN value END) AS earliest_value
FROM 
  cte
GROUP BY 
  column_name;