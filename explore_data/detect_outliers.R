#' Function to detect outliers using z-score, Tukey, Mahalanobis distance, Grubbs, median absolute deviation (MAD), and LOF methods
#'
#' @param data a data.frame object containing the data
#' @param variable a character string representing the column name of the variable of interest
#' @param z_thresh a numeric value representing the z-score threshold. Default is 1.96.
#' @param tukey_mult a numeric value representing the Tukey multiplier. Default is 1.5.
#' @param mahalanobis_thresh a numeric value representing the Mahalanobis distance threshold. Default is 0.95.
#' @param grubbs_thresh a numeric value representing the Grubbs threshold. Default is 0.05.
#' @param mad_mult a numeric value representing the MAD multiplier. Default is 3.
#' @param lof_minPts a numeric value representing the minimum number of points to be considered in a neighborhood for LOF. Default is 6.
#'
#' @return a data.frame object with additional columns for each outlier detection method
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' example_data <- data.frame(
#'   A = rnorm(100, 10, 2),
#'   B = rnorm(100, 20, 5),
#'   C = rnorm(100, 30, 10)
#' )
#' example_data[c(25, 50, 75), "B"] <- c(40, 10, 35)
#'
#' # Use function
#' results <- detect_outliers(example_data, "B")
#'
#' # Review results
#' head(results)
#'
#' @importFrom dbscan lof
#' @importFrom stats cov mahalanobis mean median pt quantile sd
#' @importFrom utils ifelse
detect_outliers <- function(data, variable, z_thresh = 1.96, tukey_mult = 1.5, mahalanobis_thresh = qchisq(0.95, ncol(data)), grubbs_thresh = 0.05, mad_mult = 3, lof_minPts = 6) {
  # Calculate z-scores
  data$z_score <- (data[,variable] - mean(data[,variable])) / sd(data[,variable])
  
  # Identify z-score outliers
  data$z_outlier <- ifelse(abs(data$z_score) > z_thresh, 1, 0)
  
  # Calculate Tukey's method
  Q1 <- quantile(data[,variable], 0.25)
  Q3 <- quantile(data[,variable], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - tukey_mult * IQR
  upper_bound <- Q3 + tukey_mult * IQR
  
  # Identify Tukey's method outliers
  data$t_outlier <- ifelse(data[,variable] < lower_bound | data[,variable] > upper_bound, 1, 0)
  
  # Calculate Mahalanobis distance
  cov_mat <- cov(data[, -which(names(data) %in% variable)])
  center <- apply(data[, -which(names(data) %in% variable)], 2, mean)
  data$md <- mahalanobis(data[, -which(names(data) %in% variable)], center, cov_mat)
  
  # Identify Mahalanobis distance outliers
  data$md_outlier <- ifelse(data$md > mahalanobis_thresh, 1, 0)
  
  # Calculate Grubbs' test
  n <- nrow(data)
  g <- (abs(data[,variable] - mean(data[,variable])) / sd(data[,variable]))
  gmax <- max(g)
  pval <- 2 * pt(gmax, n - 2, lower.tail = FALSE)
  
  # Identify Grubbs' test outlier
  data$g_outlier <- ifelse(pval < grubbs_thresh / n, 1, 0)
  
  # Calculate median absolute deviation (MAD)
  med <- median(data[,variable])
  data$mad <- abs(data[,variable] - med)
  mad_thresh <- mad_mult * median(data$mad, na.rm = TRUE)
  
  # Identify MAD outliers
  data$mad_outlier <- ifelse(data$mad > mad_thresh, 1, 0)
  
  # Calculate local outlier factor (LOF)
  library(dbscan)
  lof <- lof(data[, -which(names(data) %in% variable)], minPts = lof_minPts)
  
  # Identify LOF outliers
  data$lof_outlier <- ifelse(lof > 1, 1, 0)
  
  # Return data
  return(data)
}