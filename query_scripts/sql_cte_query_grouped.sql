-- Example of querying using a common table expression
-- Grouping data by company and state, and suming all numeric fields
-- falling between specific dates

WITH cte AS (
  SELECT company, state, SUM(CASE WHEN data_type = 'numeric' THEN value END) as sum_numeric_values
  FROM data_table
  WHERE contract_date BETWEEN '2020-02-02' AND '2023-02-02'
  GROUP BY company, state
)
SELECT *
FROM data_table
WHERE contract_date BETWEEN '2020-02-02' AND '2023-02-02'
UNION
SELECT company, state, sum_numeric_values
FROM cte;
