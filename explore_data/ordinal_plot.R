#' Creates an ordinal plot for a Likert-scale variable
#'
#' This function creates an ordinal plot for a Likert-scale variable in a given data set.
#'
#' @param data A data.frame containing the data to be analyzed.
#' @param col_name A string specifying the name of the column containing the variable to be analyzed.
#' 
#' @return An ordinal plot created using the ggplot2 package.
#'
#' @importFrom ggplot2 ggtitle
#' @importFrom likert likert
#' 
#' @examples
#' data(pisaitems)
#' ordinal_plot(pisaitems, "ST25Q01")
#'
#' @export
# Load library
library(likert)
# Save function
ordinal_plot <- function(data, col_name) {
  
  # check if col_name exists in data
  if (!col_name %in% names(data)) {
    stop("Column '", col_name, "' not found in data.")
  }
  
  # extract the column from the data set as a data frame
  col_df <- data[, col_name, drop=FALSE]
  
  # create a likert object
  l <- likert(col_df)
  
  # summarize the likert object
  ls <- likert(summary = l$results)
  
  # plot the ordinal plot
  plot(ls) + ggtitle(col_name)
}
