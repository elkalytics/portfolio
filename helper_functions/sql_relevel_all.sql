-- Relevel all categorical variables in a table
SELECT 
  CASE 
    WHEN category_count/total_count <= 0.05 THEN CONCAT(column_name, '_relvl')
    ELSE column_name
  END AS new_column_name, 
  COUNT(*) AS count
FROM my_table
CROSS JOIN (
  SELECT COUNT(*) AS total_count
  FROM my_table
) total_counts
CROSS JOIN (
  SELECT DISTINCT column_name
  FROM (
    SELECT column_name
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_NAME = 'my_table'
      AND DATA_TYPE IN ('varchar', 'text', 'char')
  ) AS cols
) AS categorical_cols
LEFT JOIN (
  SELECT 
    column_name, 
    category_value, 
    COUNT(*) AS category_count
  FROM (
    SELECT 
      column_name, 
      CASE 
        WHEN category_count/total_count <= 0.05 THEN 'Other'
        ELSE category_value
      END AS category_value
    FROM my_table
    CROSS JOIN (
      SELECT COUNT(*) AS total_count
      FROM my_table
    ) total_counts
    CROSS JOIN (
      SELECT DISTINCT column_name
      FROM (
        SELECT column_name
        FROM INFORMATION_SCHEMA.COLUMNS
        WHERE TABLE_NAME = 'my_table'
          AND DATA_TYPE IN ('varchar', 'text', 'char')
      ) AS cols
    ) AS categorical_cols
    CROSS JOIN (
      SELECT DISTINCT category_value
      FROM my_table
      WHERE column_name = 'category_column'
    ) AS category_values
    WHERE column_name = 'category_column'
  ) AS category_counts
  GROUP BY column_name, category_value
) AS counts ON my_table.category_column = counts.category_value AND categorical_cols.column_name = counts.column_name
WHERE categorical_cols.column_name IS NOT NULL
GROUP BY new_column_name
HAVING new_column_name != column_name;