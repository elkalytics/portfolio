-- Create a common table expression (CTE) called cte that joins table1 and table2 on their unique ID columns (uid)
-- and filters for rows where the character_field in table1 contains the character_field in table2 (using the LIKE operator)
-- The cte also calculates the absolute difference in days between the date_field in table1 and table2
WITH cte AS (
  SELECT a.group_number, a.character_field, b.date_field,
         ABS(DATEDIFF(day, a.date_field, b.date_field)) as date_diff
  FROM table1 a
  JOIN table2 b
  ON a.uid = b.uid
  WHERE a.character_field LIKE '%' + b.character_field + '%'
)

-- Select the group_number, character_field, and date_field from the subquery
-- The subquery calculates the row number for each uid based on the date_diff (in ascending order) and selects only the row with the smallest date_diff (row_num = 1)
SELECT group_number, character_field, date_field
FROM (
  SELECT uid, character_field, date_field,
         ROW_NUMBER() OVER (PARTITION BY uid ORDER BY date_diff) as row_num
  FROM cte
) as subquery
WHERE row_num = 1;