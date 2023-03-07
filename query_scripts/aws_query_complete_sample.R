#' Query first 10 rows without nulls or 10 most complete rows from a MySQL database
#' 
#' This function queries the first 10 rows without null values for each table in a MySQL database. If all rows in a table contain at least one null value, the function queries the 10 most complete rows in that table. The function returns a list of data frames, with each data frame containing the first 10 non-null rows or the 10 most complete rows for each table in the database.
#' 
#' @param dbname Name of the MySQL database to connect to.
#' @param user User name to use when connecting to the MySQL database.
#' @param password Password to use when connecting to the MySQL database.
#' @param host Host name or IP address of the MySQL server.
#' 
#' @return A list of data frames, with each data frame containing the first 10 non-null rows or the 10 most complete rows for each table in the database.
#' 
#' @examples
#' dbname <- "mydatabase"
#' user <- "myuser"
#' password <- "mypassword"
#' host <- "mydatabase.us-west-2.rds.amazonaws.com"
#' results <- query_first_10_rows(dbname, user, password, host)
#' 
#' @import RMySQL
# Load package
library(RMySQL)
# function to query first 10 rows of all tables without nulls or the 10 most complete rows
query_first_10_rows <- function(dbname, user, password, host) {
  # create database connection
  con <- dbConnect(MySQL(), dbname = dbname, user = user, password = password, host = host)
  
  # check if connection was successful
  if (is.null(con)) stop("Error: Could not connect to database")
  
  # get list of tables in database
  tables <- dbListTables(con)
  
  # check if any tables were found
  if (length(tables) == 0) stop("Error: No tables found in database")
  
  # create empty list to store results
  results <- vector(mode = "list", length = length(tables))
  
  # loop through tables and query the first 10 rows without nulls or the 10 most complete rows
  for (i in seq_along(tables)) {
    table <- tables[i]
    
    # get list of columns in table
    cols <- dbListFields(con, table)
    
    # create query to select rows without nulls in any column
    query_without_nulls <- sprintf("SELECT * FROM `%s` WHERE %s LIMIT 10",
                                   table, paste0("`", cols, "`", " IS NOT NULL", collapse = " AND "))
    
    # create query to select the 10 most complete rows in table
    query_most_complete <- sprintf("SELECT * FROM `%s` WHERE (%s) ORDER BY %s LIMIT 10",
                                   table, paste0("`", cols, "`", " IS NOT NULL", collapse = " OR "),
                                   paste0("(`", cols, "`", " IS NOT NULL) DESC", collapse = ", "))
    
    # try to query rows without nulls
    result <- dbGetQuery(con, query_without_nulls)
    
    # if there are nulls in every column, query the 10 most complete rows
    if (is.null(result)) {
      result <- dbGetQuery(con, query_most_complete)
      
      # if both queries fail, print a warning
      if (is.null(result)) {
        warning(sprintf("Warning: Could not query table %s", table))
      }
    }
    
    # add result to list if query was successful
    if (!is.null(result)) {
      results[[i]] <- setNames(list(result), table)
    }
  }
  
  # close database connection
  dbDisconnect(con)
  
  # return list of results
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) return(NULL)
  names(results) <- tables[names(results)]
  return(results)
}