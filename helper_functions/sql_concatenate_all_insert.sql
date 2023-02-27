-- Insert concatenated set of variables into table

INSERT INTO concatenated_data_table (concatenated_string)
SELECT CONCAT_WS('!', * ) AS concatenated_string
FROM my_data_table;
