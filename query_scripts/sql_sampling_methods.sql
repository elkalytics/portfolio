-- SQL snippest on sampling methods

-- Simple random sampling without replacement
SELECT *
FROM my_table
ORDER BY RAND()
LIMIT 100;

-- Simple random sampling with replacement
SELECT *
FROM my_table
ORDER BY RAND()
LIMIT 100
OFFSET FLOOR(RAND() * (SELECT COUNT(*) FROM my_table))
;

-- Stratified random sampling without replacement
SELECT *
FROM (
  SELECT *,
         ROW_NUMBER() OVER (PARTITION BY stratum ORDER BY RAND()) AS row_num,
         COUNT(*) OVER (PARTITION BY stratum) AS stratum_count
  FROM my_table
  WHERE stratum IS NOT NULL -- Excludes null values in the stratum column
) AS subquery
WHERE row_num <= FLOOR(size * stratum_count / (SELECT COUNT(*) FROM my_table WHERE stratum IS NOT NULL))
;

-- Stratified random sampling with replacement
SELECT *
FROM (
  SELECT *,
         ROW_NUMBER() OVER (PARTITION BY stratum ORDER BY RAND()) AS row_num,
         COUNT(*) OVER (PARTITION BY stratum) AS stratum_count
  FROM my_table
  WHERE stratum IS NOT NULL -- Excludes null values in the stratum column
) AS subquery
WHERE row_num <= CEILING(size * stratum_count / (SELECT COUNT(*) FROM my_table WHERE stratum IS NOT NULL))
;
