# SQL query most recent (based on system_date) the non-null value for each cell

WITH cte AS (
  SELECT 
    column_name,
    value,
    system_date,
    ROW_NUMBER() OVER (PARTITION BY column_name ORDER BY system_date DESC) AS rn
  FROM 
    table_name
)
SELECT 
  column_name,
  MAX(CASE WHEN value IS NOT NULL THEN system_date END) AS latest_date,
  MAX(CASE WHEN rn = 1 AND value IS NOT NULL THEN value END) AS latest_value
FROM 
  cte
GROUP BY 
  column_name;