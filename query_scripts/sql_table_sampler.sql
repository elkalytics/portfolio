-- Sample a random 10% from a data table

-- Define a common table expression (CTE) named 'cte'
WITH cte AS (
  -- Select all columns from 'your_table' and assign a random row number to each row
  SELECT *,
         ROW_NUMBER() OVER (ORDER BY random()) AS rn
  FROM your_table
)

-- Select all columns from the CTE, including only the rows with row numbers less than or equal to 10% of the total number of rows in 'your_table'
SELECT *
FROM cte
WHERE rn <= CEIL(0.1 * (SELECT COUNT(*) FROM your_table))
-- Limit the number of rows returned to 100,000 (to prevent returning an excessively large result set)
LIMIT 100000;