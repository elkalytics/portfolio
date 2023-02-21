# Extension of detect_outliers function
# Detects outliers for all numeric variables in data frame
# Saves tagged output

detect_outliers_table <- function(data, z_thresh = 1.96, tukey_mult = 1.5, mahalanobis_thresh = qchisq(0.95, ncol(data)), grubbs_thresh = 0.05, mad_mult = 3, lof_minPts = 6) {
  
  # Initialize output data frame
  results <- data.frame(row.names = row.names(data), data)
  
  # Loop through numeric columns
  for (column in names(data)[sapply(data, is.numeric)]) {
    
    # Remove missing values
    data_no_na <- data[complete.cases(data[, column]), ]
    
    # Calculate z-scores
    data_no_na$z_score <- (data_no_na[, column] - mean(data_no_na[, column])) / sd(data_no_na[, column])
    
    # Identify z-score outliers
    data_no_na$z_outlier <- ifelse(abs(data_no_na$z_score) > z_thresh, 1, 0)
    
    # Calculate Tukey's method
    Q1 <- quantile(data_no_na[, column], 0.25)
    Q3 <- quantile(data_no_na[, column], 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - tukey_mult * IQR
    upper_bound <- Q3 + tukey_mult * IQR
    
    # Identify Tukey's method outliers
    data_no_na$t_outlier <- ifelse(data_no_na[, column] < lower_bound | data_no_na[, column] > upper_bound, 1, 0)
    
    # Calculate Mahalanobis distance
    cov_mat <- cov(data_no_na[, sapply(data_no_na, is.numeric) & names(data_no_na) != column])
    center <- apply(data_no_na[, sapply(data_no_na, is.numeric) & names(data_no_na) != column], 2, mean)
    
    # Attempt to calculate Mahalanobis distance; set to NA if error occurs
    md <- try(mahalanobis(data_no_na[, sapply(data_no_na, is.numeric) & names(data_no_na) != column], center, cov_mat), silent = TRUE)
    if (class(md) == "try-error") {
      data_no_na$md_outlier <- NA
    } else {
      data_no_na$md <- md
      # Identify Mahalanobis distance outliers
      data_no_na$md_outlier <- ifelse(data_no_na$md > mahalanobis_thresh, 1, 0)
    }
    
    # Calculate Grubbs' test
    n <- nrow(data_no_na)
    g <- (abs(data_no_na[, column] - mean(data_no_na[, column])) / sd(data_no_na[, column]))
    gmax <- max(g)
    pval <- 2 * pt(gmax, n - 2, lower.tail = FALSE)
    
    # Identify Grubbs' test outlier
    data_no_na$g_outlier <- ifelse(pval < grubbs_thresh / n, 1, 0)
    
    # Calculate median absolute deviation (MAD)
    med <- median(data_no_na[, column], na.rm = TRUE)
    data_no_na$mad <- abs(data_no_na[, column] - med)
    mad_thresh <- mad_mult * median(data_no_na$mad, na.rm = TRUE)
    
    # Identify MAD outliers
    data_no_na$mad_outlier <- ifelse(data_no_na$mad > mad_thresh, 1, 0)
    
    # Calculate local outlier factor (LOF)
    if (sum(!is.na(data_no_na[, column])) >= lof_minPts) {
      # Attempt to calculate LOF; set to NA if error occurs
      lof <- try(lof(data_no_na[, sapply(data_no_na, is.numeric) & names(data_no_na) != column], minPts = lof_minPts), silent = TRUE)
      if (class(lof) == "try-error") {
        data_no_na$lof_outlier <- NA
      } else {
        # Identify LOF outliers
        data_no_na$lof_outlier <- ifelse(lof > 1, 1, 0)
      }
    } else {
      data_no_na$lof_outlier <- NA
    }
    
    # Combine results with original data
    results[paste0(column, "_original")] <- data[, column]
    results[paste0(column, "_z_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$z_outlier)
    results[paste0(column, "_t_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$t_outlier)
    results[paste0(column, "_md_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$md_outlier)
    results[paste0(column, "_g_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$g_outlier)
    results[paste0(column, "_mad_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$mad_outlier)
    results[paste0(column, "_lof_outlier")] <- ifelse(!complete.cases(data[, column]), NA, data_no_na$lof_outlier)
  }
  
  # Return results
  return(results)
}

    