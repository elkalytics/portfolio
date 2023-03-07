-- Create a table called my_table with four columns
CREATE TABLE IF NOT EXISTS my_table (
    id INT PRIMARY KEY,
    column1 VARCHAR(255),
    column2 VARCHAR(255),
    column3 VARCHAR(255)
);

-- Create a table called my_table_changes to store changes made to my_table
CREATE TABLE IF NOT EXISTS my_table_changes (
    id INT PRIMARY KEY AUTO_INCREMENT,
    table_name VARCHAR(255),
    column_name VARCHAR(255),
    old_value VARCHAR(255),
    new_value VARCHAR(255),
    change_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Change the delimiter to // so that semicolons within the trigger don't cause errors
DELIMITER //

-- Create a trigger called my_table_trigger that fires after an update is made to my_table
CREATE TRIGGER my_table_trigger AFTER UPDATE ON my_table
FOR EACH ROW
BEGIN
    -- Create a temporary table to store the changes made to my_table
    CREATE TEMPORARY TABLE IF NOT EXISTS temp_table (
        column_name VARCHAR(255),
        old_value VARCHAR(255),
        new_value VARCHAR(255)
    );
    
    -- Insert the column name, old value, and new value for each column in my_table that was updated into the temporary table
    INSERT INTO temp_table (column_name, old_value, new_value)
    SELECT COLUMN_NAME(OLD, col), OLD[COLUMN_NAME(OLD, col)], NEW[COLUMN_NAME(OLD, col)]
    FROM DUAL
    -- Exclude the 'id' column, as it is a primary key and should not be updated
    WHERE COLUMN_NAME(OLD, col) NOT IN ('id') AND OLD[COLUMN_NAME(OLD, col)] <> NEW[COLUMN_NAME(OLD, col)]
    FOR col IN 1..COLUMN_COUNT(OLD) DO END FOR;

    -- Insert the table name, column name, old value, and new value for each change into the my_table_changes table
    INSERT INTO my_table_changes (table_name, column_name, old_value, new_value)
    SELECT 'my_table', column_name, old_value, new_value FROM temp_table;
    
    -- Drop the temporary table after the trigger has finished executing
    DROP TEMPORARY TABLE IF EXISTS temp_table;
END //

-- Reset the delimiter to the default semicolon
DELIMITER ;