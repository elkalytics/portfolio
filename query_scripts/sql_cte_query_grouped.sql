-- Example of querying using a common table expression
-- Grouping data by company and state, and summing all numeric fields
-- falling between specific dates

-- Define a common table expression (CTE) named 'cte'
WITH cte AS (
  -- Select the company, state, and sum of numeric values (if data_type is 'numeric')
  -- from the 'data_table' where the contract date is between '2020-02-02' and '2023-02-02',
  -- and group the results by company and state
  SELECT company, state, SUM(CASE WHEN data_type = 'numeric' THEN value END) as sum_numeric_values
  FROM data_table
  WHERE contract_date BETWEEN '2020-02-02' AND '2023-02-02'
  GROUP BY company, state
)

-- Combine the results of the CTE with the original 'data_table' using a UNION operation
SELECT *
FROM data_table
WHERE contract_date BETWEEN '2020-02-02' AND '2023-02-02'
UNION
SELECT company, state, sum_numeric_values
FROM cte;
