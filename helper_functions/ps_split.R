#' Split columns by prefix or suffix
#'
#' This function splits the columns of a data frame by a common prefix or suffix and returns a list of data frames. 
#' 
#' @param data A data frame to split
#' @param by A character string specifying whether to split by prefix or suffix. Must be either "prefix" or "suffix".
#' @param delimiter A character string specifying the delimiter used to separate the prefix or suffix from the rest of the column name. Defaults to "_".
#' @param id A character string specifying the name of the column to use as an index variable. If not provided, the function will create a new column named "id" with row numbers.
#'
#' @return A list of data frames, where each data frame contains columns with a common prefix or suffix.
#'
#' @examples
#' data <- data.frame(
#'   id = 1:5,
#'   x_a = rnorm(5),
#'   x_b = rnorm(5),
#'   y_a = rnorm(5),
#'   y_b = rnorm(5)
#' )
#' ps_split(data, by = "suffix", delimiter = "_", id = "id")
#'
#' @export
ps_split <- function(data, by = "prefix", delimiter = "_", id = NULL) {
  # If no id is specified, add a row number column
  if (is.null(id)) {
    data$id <- seq_len(nrow(data))
    id <- "id"
  }
  
  # Get column names
  colnames <- names(data)
  
  # Initialize list to hold data frames
  data_frames <- list()
  
  # Loop through column names
  for (i in 1:length(colnames)) {
    # Split column name by delimiter
    split_colname <- strsplit(colnames[i], delimiter)[[1]]
    
    # Check if splitting by prefix or suffix
    if (by == "prefix") {
      prefix <- split_colname[1]
      suffix <- paste0(delimiter, paste(split_colname[-1], collapse = delimiter))
    } else if (by == "suffix") {
      prefix <- paste0(delimiter, paste(split_colname[-length(split_colname)], collapse = delimiter))
      suffix <- split_colname[length(split_colname)]
    } else {
      stop("Invalid value for 'by'. Must be 'prefix' or 'suffix'.")
    }
    
    # Check if prefix or suffix already exists as a data frame
    if (prefix %in% names(data_frames)) {
      # Append column to existing data frame
      data_frames[[prefix]] <- cbind(data_frames[[prefix]], data[, c(id, colnames[i])])
    } else if (suffix %in% names(data_frames)) {
      # Append column to existing data frame
      data_frames[[suffix]] <- cbind(data_frames[[suffix]], data[, c(id, colnames[i])])
    } else {
      # Create new data frame
      data_frames[[suffix]] <- data.frame(data[, c(id, colnames[i])])
      names(data_frames)[length(data_frames)] <- suffix
    }
  }
  
  return(data_frames)
}