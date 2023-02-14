-- Example T-SQL query that fuzzy joins on character strings and 
-- the nearest date between two tables by

WITH cte AS (
  SELECT a.group_number, a.character_field, b.date_field,
         ABS(DATEDIFF(day, a.date_field, b.date_field)) as date_diff
  FROM table1 a
  JOIN table2 b
  ON a.uid = b.uid
  WHERE a.character_field LIKE '%' + b.character_field + '%'
)
SELECT group_number, character_field, date_field
FROM (
  SELECT uid, character_field, date_field,
         ROW_NUMBER() OVER (PARTITION BY uid ORDER BY date_diff) as row_num
  FROM cte
) as subquery
WHERE row_num = 1;
