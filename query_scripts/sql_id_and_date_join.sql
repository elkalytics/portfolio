-- Join two tables using an ID and a date if the date is +/- 7 days

-- Select all columns from both tables
SELECT 
  t1.*, 
  t2.*
FROM 
  -- Specify the first table as 't1' and the second table as 't2'
  table1 t1
  -- Join the two tables on the 'id' column and the difference in dates is within 7 days
  JOIN table2 t2 
    ON t1.id = t2.id
    AND DATEDIFF(day, t1.date, t2.date) BETWEEN -7 and 7;
