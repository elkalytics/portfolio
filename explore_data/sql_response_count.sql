# SQL query a table to determine if there is no response (0), a single value (1), or multiple values
# Script will count the number of responses if more than one

WITH cte AS (
  SELECT 
    column_name,
    COUNT(DISTINCT value) AS value_count
  FROM 
    table_name
  WHERE 
    value IS NOT NULL
  GROUP BY 
    column_name
)
SELECT 
  column_name,
  CASE 
    WHEN value_count = 0 THEN 'No Responses'
    WHEN value_count = 1 THEN 'Single Value'
    ELSE CONCAT(value_count, ' Different Values')
  END AS outcome
FROM 
  cte;