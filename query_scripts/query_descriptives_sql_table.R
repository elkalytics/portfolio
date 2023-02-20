library(RMySQL)

# Specify database connection details
dsn <- "your_dsn_name"
user <- "your_username"
password <- "your_password"

# Connect to the database
con <- dbConnect(MySQL(), user=user, password=password, dbname=dsn)

# Specify the table name
table_name <- "your_table_name"

# Construct the SQL query to get statistics for numeric columns
sql_query <- paste0("SELECT COLUMN_NAME,
                     AVG(CASE WHEN ", table_name, ".`COLUMN_NAME` REGEXP '^[0-9]+\\.?[0-9]*$' THEN CAST(", table_name, ".`COLUMN_NAME` AS DECIMAL) ELSE NULL END) AS mean,
                     PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY CASE WHEN ", table_name, ".`COLUMN_NAME` REGEXP '^[0-9]+\\.?[0-9]*$' THEN CAST(", table_name, ".`COLUMN_NAME` AS DECIMAL) ELSE NULL END) AS median,
                     SUBSTRING_INDEX(GROUP_CONCAT(CASE WHEN ", table_name, ".`COLUMN_NAME` REGEXP '^[0-9]+\\.?[0-9]*$' THEN CAST(", table_name, ".`COLUMN_NAME` AS DECIMAL) END ORDER BY ", table_name, ".`COLUMN_NAME` ASC SEPARATOR ','), ',', 1) AS mode,
                     MAX(CASE WHEN ", table_name, ".`COLUMN_NAME` REGEXP '^[0-9]+\\.?[0-9]*$' THEN CAST(", table_name, ".`COLUMN_NAME` AS DECIMAL) ELSE NULL END) AS max,
                     MIN(CASE WHEN ", table_name, ".`COLUMN_NAME` REGEXP '^[0-9]+\\.?[0-9]*$' THEN CAST(", table_name, ".`COLUMN_NAME` AS DECIMAL) ELSE NULL END) AS min,
                     COUNT(CASE WHEN ", table_name, ".`COLUMN_NAME` IS NOT NULL THEN 1 ELSE NULL END) AS count,
                     COUNT(CASE WHEN ", table_name, ".`COLUMN_NAME` IS NOT NULL THEN 1 ELSE NULL END) / COUNT(*) AS response_percentage
              FROM information_schema.columns
              JOIN ", table_name, " ON information_schema.columns.TABLE_SCHEMA=DATABASE() AND information_schema.columns.TABLE_NAME='", table_name, "' AND information_schema.columns.COLUMN_NAME = '", table_name, "`.`COLUMN_NAME`
              WHERE information_schema.columns.DATA_TYPE IN ('decimal', 'int', 'float', 'double')
              GROUP BY COLUMN_NAME")

# Execute the SQL query and fetch the results
results <- dbGetQuery(con, sql_query)

# Print the results
print(results)

# Close the database connection
dbDisconnect(con)
