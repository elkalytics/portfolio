-- Creates a database trigger named trg_log_changes that logs changes to a table named your_table into a historical data table named historical_data_table
-- If the unique_id does not exist, a new row is inserted into the historical_data_table with the unique_id
-- If the unique_id already exists in the historical_data_table, the trigger constructs a select query to retrieve the current row in your_table that corresponds to the unique_id and the most recent sys_time in the historical_data_table
-- Uses several variables such as @column_list, @value_list, @current_row_query, and @select_query to construct the select and insert queries dynamically
-- The information_schema.columns table is used to retrieve the column names of your_table dynamically, except for the unique_id column
-- Trigger is designed to log changes to a specific table named your_table

DELIMITER $$
CREATE TRIGGER trg_log_changes 
AFTER INSERT OR UPDATE ON your_table
FOR EACH ROW 
BEGIN
    DECLARE @id_count INT;
    DECLARE @column_list VARCHAR(1000);
    DECLARE @value_list VARCHAR(1000);
    DECLARE @current_row_query VARCHAR(1000);
    DECLARE @current_row_exists INT DEFAULT 0;
    DECLARE @select_query VARCHAR(1000);

    -- check if ID already exists in historical data table
    SELECT COUNT(*) INTO @id_count FROM historical_data_table WHERE unique_id = NEW.unique_id;

    IF @id_count = 0 THEN
        -- new record
        INSERT INTO historical_data_table (unique_id, sys_time, ...)
        SELECT NEW.unique_id, NOW(), ... FROM DUAL;
    ELSE
        -- check for changes
        SET @column_list := (SELECT GROUP_CONCAT(CONCAT(column_name, ', ')) 
                             FROM information_schema.columns 
                             WHERE table_name = 'your_table' AND column_name <> 'unique_id' AND table_schema = DATABASE());
        SET @column_list := TRIM(TRAILING ', ' FROM @column_list);

        SET @value_list := (SELECT GROUP_CONCAT(CONCAT('NEW.', column_name, ', ')) 
                            FROM information_schema.columns 
                            WHERE table_name = 'your_table' AND column_name <> 'unique_id' AND table_schema = DATABASE());
        SET @value_list := TRIM(TRAILING ', ' FROM @value_list);

        -- construct select query to get current row
        SET @current_row_query := CONCAT('SELECT ', @column_list, ' FROM your_table WHERE unique_id = ', NEW.unique_id, ' AND sys_time = (SELECT MAX(sys_time) FROM historical_data_table WHERE unique_id = ', NEW.unique_id, ')');

        BEGIN
            DECLARE EXIT HANDLER FOR NOT FOUND SET @current_row_exists := 0;
            SELECT 1 INTO @current_row_exists FROM DUAL WHERE EXISTS (SELECT * FROM (SELECT @current_row_query) t WHERE EXISTS (SELECT * FROM historical_data_table WHERE unique_id = NEW.unique_id));
        END;

        IF @current_row_exists = 0 THEN
            -- log changes
            SET @select_query := CONCAT('INSERT INTO historical_data_table (unique_id, sys_time, ', @column_list, ') SELECT NEW.unique_id, NOW(), ', @value_list, ' FROM DUAL');
            PREPARE stmt FROM @select_query;
            EXECUTE stmt;
            DEALLOCATE PREPARE stmt;
        END IF;
    END IF;
END$$
DELIMITER ;
