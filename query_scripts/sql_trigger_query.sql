-- Check if a change has occured
-- Update if one has

CREATE TABLE IF NOT EXISTS my_table (
    id INT PRIMARY KEY,
    column1 VARCHAR(255),
    column2 VARCHAR(255),
    column3 VARCHAR(255)
);

CREATE TABLE IF NOT EXISTS my_table_changes (
    id INT PRIMARY KEY AUTO_INCREMENT,
    table_name VARCHAR(255),
    column_name VARCHAR(255),
    old_value VARCHAR(255),
    new_value VARCHAR(255),
    change_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

DELIMITER //

CREATE TRIGGER my_table_trigger AFTER UPDATE ON my_table
FOR EACH ROW
BEGIN
    INSERT INTO my_table_changes (table_name, column_name, old_value, new_value)
    SELECT 'my_table', COLUMN_NAME(OLD, col), OLD[COLUMN_NAME(OLD, col)], NEW[COLUMN_NAME(OLD, col)]
    FROM DUAL
    WHERE COLUMN_NAME(OLD, col) NOT IN ('id') AND OLD[COLUMN_NAME(OLD, col)] <> NEW[COLUMN_NAME(OLD, col)]
    FOR col IN 1..COLUMN_COUNT(OLD) DO END FOR;
END //

DELIMITER ;
