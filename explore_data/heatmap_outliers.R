#' Generate a heatmap of outlier values in a data frame
#'
#' This function generates a heatmap of outlier values in a data frame. The data frame should have columns that
#' contain the string "outlier" in their names.
#'
#' @param df A data frame containing outlier values
#'
#' @return A heatmap showing the distribution of outlier values in the data frame
#'
#' @examples
#' data <- data.frame(x_outlier = c(1, 2, 3), y_outlier = c(4, 5, 6))
#' heatmap_outliers(data)
#'
#' @importFrom graphics heatmap
#' @importFrom grDevices colorRampPalette
heatmap_outliers <- function(df) {
  outlier_cols <- grep("outlier", colnames(df))
  df_num <- df[outlier_cols]
  df_num[is.na(df_num)] <- 0 # replace NAs with 0
  df_num <- apply(df_num, 2, as.numeric) # convert to numeric matrix
  colnames(df_num) <- gsub("_outlier", "", colnames(df_num))
  heatmap(df_num, Rowv = NA, Colv = NA, col = colorRampPalette(c("white", "red"))(100), scale = "none",
          labRow = FALSE, labCol = colnames(df_num), main = "Outliers Heatmap")
}