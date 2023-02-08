# Join two tables using an ID and a date if the date is +/- 7 days
SELECT 
  t1.*, 
  t2.*
FROM 
  table1 t1
  JOIN table2 t2 
    ON t1.id = t2.id
    AND DATEDIFF(day, t1.date, t2.date) BETWEEN -7 and 7;
