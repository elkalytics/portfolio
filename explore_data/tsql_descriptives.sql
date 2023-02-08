# T-SQL descriptive statistic examples
# Need better calculations for skewness and kurtosis
# Probably better to do those in R

WITH Data AS (
  SELECT Value, ROW_NUMBER() OVER (ORDER BY Value) AS RowNum
  FROM MyTable
),
Counts AS (
  SELECT COUNT(Value) AS [Count]
  FROM MyTable
),
Mean AS (
  SELECT AVG(Value) AS MeanValue
  FROM MyTable
),
SumSquaredDiff AS (
  SELECT SUM((Value - MeanValue) * (Value - MeanValue)) AS SumSquaredDiff
  FROM Mean CROSS JOIN MyTable
),
StdDev AS (
  SELECT SQRT(SumSquaredDiff.SumSquaredDiff / ([Count] - 1)) AS StdDev
  FROM SumSquaredDiff CROSS JOIN Counts
),
Mode AS (
  SELECT TOP 1 Value AS ModeValue, COUNT(Value) AS ModeCount
  FROM MyTable
  GROUP BY Value
  ORDER BY ModeCount DESC
),
Median AS (
  SELECT Value AS MedianValue
  FROM Data
  WHERE RowNum IN (
    SELECT (([Count] + 1) / 2)
    FROM Counts
  )
),
Skewness AS (
  SELECT SUM((Value - MeanValue) / StdDev.StdDev) * (3 / [Count]) AS Skewness
  FROM MyTable CROSS JOIN StdDev
),
Kurtosis AS (
  SELECT SUM(((Value - MeanValue) / StdDev.StdDev) * ((Value - MeanValue) / StdDev.StdDev) - 3) * (
    -3 / ([Count] - 1)
  ) + 3 AS Kurtosis
  FROM MyTable CROSS JOIN StdDev
)
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
