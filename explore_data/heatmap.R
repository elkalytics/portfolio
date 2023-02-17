
library(reshape2)

create_heatmap <- function(data, var1, var2) {
  # Subset the data frame to only include the specified variables
  df_subset <- data[, c(var1, var2)]
  
  # Create a matrix of the data, with var1 as the rows and var2 as the columns
  matrix_data <- suppressWarnings(as.matrix(dcast(df_subset, get(var1) ~ get(var2), value.var = var2, fun.aggregate = length)))
  
  # Create a custom color palette that goes from a very light yellow to a lighter shade of blue
  my_palette <- colorRampPalette(c("#FFFFCC", "lightblue"))(256)
  
  # Create the heatmap using the matrix of data and the custom color palette, with numeric values displayed
  heatmap(matrix_data, 
          Rowv = NA, Colv = NA, 
          col = my_palette, 
          scale = "column",
          margins = c(5, 10),
          labCol = colnames(matrix_data),
          labRow = rownames(matrix_data),
          cexRow = 0.8,
          cexCol = 0.8,
          main = "Heatmap",
          cex.main = 1.2,
          useRaster = TRUE)
  
  # Add numeric values to the cells of the heatmap
  text(x = rep(seq_along(colnames(matrix_data)), each = nrow(matrix_data)), 
       y = rep(seq_along(row.names(matrix_data)), times = ncol(matrix_data)), 
       labels = round(matrix_data, digits = 2), 
       col = "black")
}


## Load some example data
# data(mtcars)

## Create a heatmap of mpg and wt
# create_heatmap(mtcars, "cyl", "carb")


