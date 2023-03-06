library(RPostgreSQL)

# Set up database connection
con <- dbConnect(PostgreSQL(),
                 dbname = "your_database_name",
                 host = "your_host",
                 port = "your_port",
                 user = "your_username",
                 password = "your_password")

# Query sample of 10 rows from each table
sample_data <- dbGetQuery(con, "
  SELECT * FROM (
    SELECT table_name, column_name FROM information_schema.columns
    WHERE table_schema = 'public'
    ORDER BY table_name, column_name
  ) AS cols
  JOIN (
    SELECT table_name, *
    FROM (
      SELECT table_name, ROW_NUMBER() OVER (ORDER BY RANDOM()) as rownum
      FROM information_schema.tables
      WHERE table_schema = 'public'
    ) AS rndm
    JOIN information_schema.tables t ON t.table_name = rndm.table_name
    WHERE rndm.rownum <= 10
  ) AS tabs ON cols.table_name = tabs.table_name
  ORDER BY tabs.table_name, cols.column_name")

# Close database connection
dbDisconnect(con)

# View sample data
print(sample_data)