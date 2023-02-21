# Heatmap function used with the detect_outliers_table function
# Shows outliers in heatmap form - still under testing

heatmap_outliers <- function(df) {
  outlier_cols <- grep("outlier", colnames(df))
  df_num <- df[outlier_cols]
  df_num[is.na(df_num)] <- 0 # replace NAs with 0
  df_num <- apply(df_num, 2, as.numeric) # convert to numeric matrix
  colnames(df_num) <- gsub("_outlier", "", colnames(df_num))
  heatmap(df_num, Rowv = NA, Colv = NA, col = colorRampPalette(c("white", "red"))(100), scale = "none",
          labRow = FALSE, labCol = colnames(df_num), main = "Outliers Heatmap")
}