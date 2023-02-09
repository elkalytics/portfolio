# Sample a random 10% from a data table

WITH cte AS (
  SELECT *,
         ROW_NUMBER() OVER (ORDER BY random()) AS rn
  FROM your_table
)
SELECT *
FROM cte
WHERE rn <= CEIL(0.1 * (SELECT COUNT(*) FROM your_table))
LIMIT 100000;