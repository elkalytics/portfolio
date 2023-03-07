-- SQL snippet on sampling methods

-- Simple random sampling without replacement
SELECT *
FROM my_table
-- Order the results by a random number
ORDER BY RAND()
-- Limit the number of rows returned to 100
LIMIT 100;

-- Simple random sampling with replacement
SELECT *
FROM my_table
-- Order the results by a random number
ORDER BY RAND()
-- Limit the number of rows returned to 100, randomly selecting with replacement
LIMIT 100
-- Offset the starting point for the limit by a random integer value (up to the total number of rows in the table)
OFFSET FLOOR(RAND() * (SELECT COUNT(*) FROM my_table))
;

-- Stratified random sampling without replacement
SELECT *
FROM (
  SELECT *,
         -- Assign a row number to each row within its stratum, ordered by a random number
         ROW_NUMBER() OVER (PARTITION BY stratum ORDER BY RAND()) AS row_num,
         -- Count the number of rows in each stratum
         COUNT(*) OVER (PARTITION BY stratum) AS stratum_count
  FROM my_table
  -- Exclude rows with null values in the stratum column
  WHERE stratum IS NOT NULL
) AS subquery
-- Limit the number of rows returned for each stratum based on the desired sample size
WHERE row_num <= FLOOR(size * stratum_count / (SELECT COUNT(*) FROM my_table WHERE stratum IS NOT NULL))
;

-- Stratified random sampling with replacement
SELECT *
FROM (
  SELECT *,
         -- Assign a row number to each row within its stratum, ordered by a random number
         ROW_NUMBER() OVER (PARTITION BY stratum ORDER BY RAND()) AS row_num,
         -- Count the number of rows in each stratum
         COUNT(*) OVER (PARTITION BY stratum) AS stratum_count
  FROM my_table
  -- Exclude rows with null values in the stratum column
  WHERE stratum IS NOT NULL
) AS subquery
-- Limit the number of rows returned for each stratum based on the desired sample size
WHERE row_num <= CEILING(size * stratum_count / (SELECT COUNT(*) FROM my_table WHERE stratum IS NOT NULL))
;