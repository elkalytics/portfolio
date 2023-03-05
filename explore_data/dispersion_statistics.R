#' Calculate measures of dispersion for every numeric column in a dataframe
#' 
#' This function calculates a variety of measures of dispersion for every numeric column in a dataframe.
#' 
#' @param data A dataframe with numeric columns
#' 
#' @return A dataframe with additional columns for each calculated statistic
#' 
#' @details This function calculates the following statistics for each numeric column in the input dataframe:
#' \itemize{
#'  \item \code{range_val}: the range of the column
#'  \item \code{iqr_val}: the interquartile range of the column
#'  \item \code{var_val}: the variance of the column
#'  \item \code{sd_val}: the standard deviation of the column
#'  \item \code{mad_val}: the median absolute deviation of the column
#'  \item \code{median_ad_val}: the median absolute deviation from the median of the column
#'  \item \code{cv_val}: the coefficient of variation of the column
#' }
#' 
#' @examples
#' # Make fake data
#' set.seed(123)
#' df <- data.frame(
#'   var1 = rnorm(100, 10, 2),
#'   var2 = rnorm(100, 20, 4),
#'   var3 = rnorm(100, 30, 6),
#'   var4 = sample(c(0, 1, NA), 100, replace = TRUE),
#'   var5 = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#' 
#' # Calculate dispersion statistics
#' dispersion_statistics(df)
#' 
#' @import dplyr
#' 
#' @export
dispersion_statistics <- function(data) {
  library(dplyr)
  
  # Loop through each numeric column
  for (col in names(data)[sapply(data, is.numeric)]) {
    # Calculate statistics
    range_val <- range(data[[col]], na.rm = TRUE)
    iqr_val <- IQR(data[[col]], na.rm = TRUE)
    var_val <- var(data[[col]], na.rm = TRUE)
    sd_val <- sd(data[[col]], na.rm = TRUE)
    mad_val <- mad(data[[col]], na.rm = TRUE)
    median_ad_val <- median(abs(data[[col]] - median(data[[col]], na.rm = TRUE)), na.rm = TRUE)
    cv_val <- sd_val / mean(data[[col]], na.rm = TRUE)
    
    # Add variables to data set
    data[[paste0(col, "_range")]] <- range_val
    data[[paste0(col, "_iqr")]] <- iqr_val
    data[[paste0(col, "_var")]] <- var_val
    data[[paste0(col, "_sd")]] <- sd_val
    data[[paste0(col, "_mad")]] <- mad_val
    data[[paste0(col, "_median_ad")]] <- median_ad_val
    data[[paste0(col, "_cv")]] <- cv_val
  }
  
  # Return updated data set
  return(data)
}