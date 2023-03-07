#' Query Salesforce Report Data
#' 
#' This function retrieves data from a Salesforce report by querying it in batches of 2000 rows.
#' 
#' @param report_id The ID of the report to query.
#' @param session An authenticated session object that has permission to query the report.
#' 
#' @return A data frame containing the report data.
#' 
#' @import salesforcer
#' @export
# Load package
library(salesforcer)
# Define function to query report data in batches
query_report_data <- function(report_id, session) {
  # Initialize variables
  all_data <- data.frame()
  query_result <- NULL
  done <- FALSE
  
  # Query report data in batches of 2000 rows
  while (!done) {
    # Query report data
    query_result <- if (is.null(query_result)) {
      sf_query(session, paste0("SELECT ", 
                               paste(report_metadata(report_id)$detailColumns$name, collapse = ","),
                               " FROM ",
                               report_metadata(report_id)$reportMetadata.reportType.fullName,
                               " ORDER BY ",
                               report_metadata(report_id)$reportMetadata.sortColumn,
                               " ASC",
                               " LIMIT 2000"),
               "query")
    } else {
      query_result <- sf_query_more(session, query_result$nextRecordsUrl)
    }
    
    # Add query result to data frame
    query_data <- as.data.frame(query_result$results)
    all_data <- rbind(all_data, query_data)
    
    # Check if more data is available
    done <- query_result$done
  }
  
  # Return all data as a data frame
  return(all_data)
}