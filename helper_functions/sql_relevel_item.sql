-- Relevel specific categorical variable
-- Only return new variable if re-level is needed based on threshold
-- Update name and table

--- Steps
----- Create a temporary table "counts" counting number of times each category appears in the original variable
----- Create another temporary table "total" that sums the from "counts" to get the total count of all categories
----- Create a third temporary table "relabeled" that relabels categories with counts below the threshold to "Other"
----- Create two more temporary tables, "original_levels" and "new_levels", that list the original categories and the new categories
----- Create a fifth temporary table "should_relabel" that checks whether any categories actually need to be relabeled, and whether doing so would result in all categories being collapsed into a single "Other" category
----- Finally, select the new variable name and value, using the original variable name if no relabeling is necessary, and adding "_relvl" to the end if relabeling is necessary

WITH counts AS (
  SELECT original_variable, COUNT(*) AS n
  FROM table_name
  GROUP BY original_variable
),
total AS (
  SELECT SUM(n) AS total_count
  FROM counts
),
relabeled AS (
  SELECT 
    CASE 
      WHEN n/total_count >= 0.05 THEN original_variable
      ELSE 'Other'
    END AS new_category,
    COUNT(*) AS count
  FROM table_name
  JOIN counts ON table_name.original_variable = counts.original_variable
  JOIN total ON 1=1
  GROUP BY new_category
),
original_levels AS (
  SELECT original_variable
  FROM counts
),
new_levels AS (
  SELECT new_category
  FROM relabeled
  WHERE new_category <> 'Other'
),
should_relabel AS (
  SELECT 
    COUNT(DISTINCT original_variable) AS original_count,
    COUNT(DISTINCT new_category) AS new_count
  FROM original_levels
  FULL OUTER JOIN new_levels ON 1=1
)
SELECT 
  CASE 
    WHEN new_count < original_count AND new_count > 1 THEN original_variable || '_relvl'
    ELSE original_variable
  END AS new_variable,
  COALESCE(new_category, original_variable) AS new_value
FROM table_name
LEFT JOIN relabeled ON table_name.original_variable = relabeled.original_variable