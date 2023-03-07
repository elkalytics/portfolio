-- Dynamic T-SQL to query descriptives for every variable
-- Not tested/run

-- Declare variables
DECLARE @sql NVARCHAR(MAX) = '';
DECLARE @columns NVARCHAR(MAX) = '';
DECLARE @columnType NVARCHAR(MAX) = '';

-- Get a list of numeric columns in the table
SELECT @columns += ',' + COLUMN_NAME
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME = 'MyTable' AND DATA_TYPE IN ('int', 'decimal', 'float', 'numeric', 'real')

-- Get the data type of each column
SELECT @columnType += ',' + DATA_TYPE
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME = 'MyTable' AND DATA_TYPE IN ('int', 'decimal', 'float', 'numeric', 'real')

-- Generate the T-SQL code for each column
SET @sql = 'WITH Data AS (
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
SELECT ''ColumnName'',
(
  SELECT MeanValue
  FROM Mean
) AS 
Mean,
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
SELECT StdDev
FROM StdDev
) AS StdDev,
(
SELECT Skewness
FROM Skewness
) AS Skewness,
(
SELECT Kurtosis
FROM Kurtosis
) AS Kurtosis
FROM (
SELECT Value, ''' + SUBSTRING(@columns, 2, LEN(@columns)) + ''' AS ColumnName
FROM MyTable
) t
PIVOT (
SUM(Value)
FOR ColumnName IN ( ' + SUBSTRING(@columns, 2, LEN(@columns)) + ' )
) p';

-- Execute the T-SQL code
EXECUTE sp_executesql @sql;