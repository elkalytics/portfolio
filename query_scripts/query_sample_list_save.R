# Sample from every table in a schema saving results in a list

library(RPostgreSQL)

get_tables <- function(conn) {
  dbGetQuery(conn, "SELECT table_name
                   FROM information_schema.tables
                   WHERE table_schema = 'public'
                   AND table_type = 'BASE TABLE'")
}

sample_table <- function(conn, table_name) {
  query <- paste0("WITH cte AS (
                     SELECT *,
                            ROW_NUMBER() OVER (ORDER BY random()) AS rn
                     FROM ", table_name, "
                   )
                   SELECT *
                   FROM cte
                   WHERE rn <= CEIL(0.1 * (SELECT COUNT(*) FROM ", table_name, "))
                   LIMIT 100000;")
  dbGetQuery(conn, query)
}

sample_schema <- function(conn) {
  tables <- get_tables(conn)
  samples <- list()
  for (table in tables$table_name) {
    samples[[table]] <- sample_table(conn, table)
  }
  samples
}

conn <- dbConnect(
  dbDriver("PostgreSQL"),
  host="your_host",
  dbname="your_database",
  user="your_user",
  password="your_password"
)
samples <- sample_schema(conn)
dbDisconnect(conn)
