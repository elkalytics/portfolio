-- T-SQL descriptive statistic examples
-- Need better calculations for skewness and kurtosis
-- Probably better to do those in R

-- Create a common table expression (CTE) named Data that adds a row number to each value in MyTable
WITH Data AS (
  SELECT Value, ROW_NUMBER() OVER (ORDER BY Value) AS RowNum
  FROM MyTable
),

-- Create a CTE named Counts that calculates the count of non-null values in MyTable
Counts AS (
  SELECT COUNT(Value) AS [Count]
  FROM MyTable
),

-- Create a CTE named Mean that calculates the average value in MyTable
Mean AS (
  SELECT AVG(Value) AS MeanValue
  FROM MyTable
),

-- Create a CTE named SumSquaredDiff that calculates the sum of squared differences from the mean in MyTable
SumSquaredDiff AS (
  SELECT SUM((Value - MeanValue) * (Value - MeanValue)) AS SumSquaredDiff
  FROM Mean CROSS JOIN MyTable
),

-- Create a CTE named StdDev that calculates the standard deviation of values in MyTable
StdDev AS (
  SELECT SQRT(SumSquaredDiff.SumSquaredDiff / ([Count] - 1)) AS StdDev
  FROM SumSquaredDiff CROSS JOIN Counts
),

-- Create a CTE named Mode that calculates the mode (most frequent value) in MyTable
Mode AS (
  SELECT TOP 1 Value AS ModeValue, COUNT(Value) AS ModeCount
  FROM MyTable
  GROUP BY Value
  ORDER BY ModeCount DESC
),

-- Create a CTE named Median that calculates the median value in MyTable
Median AS (
  SELECT Value AS MedianValue
  FROM Data
  WHERE RowNum IN (
    SELECT (([Count] + 1) / 2)
    FROM Counts
  )
),

-- Create a CTE named Skewness that calculates the skewness of values in MyTable
Skewness AS (
  SELECT SUM((Value - MeanValue) / StdDev.StdDev) * (3 / [Count]) AS Skewness
  FROM MyTable CROSS JOIN StdDev
),

-- Create a CTE named Kurtosis that calculates the kurtosis of values in MyTable
Kurtosis AS (
  SELECT SUM(((Value - MeanValue) / StdDev.StdDev) * ((Value - MeanValue) / StdDev.StdDev) - 3) * (
    -3 / ([Count] - 1)
  ) + 3 AS Kurtosis
  FROM MyTable CROSS JOIN StdDev
)

-- Select the calculated statistics from each CTE
SELECT (
  SELECT MeanValue
  FROM Mean
) AS Mean,
(
  SELECT StdDev.StdDev
  FROM StdDev
) AS StandardDeviation,
(
  SELECT ModeValue
  FROM Mode
) AS Mode,
(
  SELECT MedianValue
  FROM Median
) AS Median,
(
  SELECT MIN(Value)
  FROM MyTable
) AS Minimum,
(
  SELECT MAX(Value)
  FROM MyTable
) AS Maximum,
(
  SELECT Skewness.Skewness
  FROM Skewness
) AS Skewness,
(
  SELECT Kurtosis.Kurtosis
  FROM Kurtosis
) AS Kurtosis
