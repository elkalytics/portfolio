# Load packages
library(ggplot2)
library(dplyr)

# Function for bar counts, sums, means, medians, min, and max
# Combines all the individual barplot functions into one
barplot_stat <- function(data, x, y = NULL, stat_type = NULL) {
  stat_functions <- c("sum", "mean", "median", "min", "max")
  
  if (!is.null(stat_type) && !stat_type %in% stat_functions) {
    stop("Invalid stat_type. Choose one of the following: sum, mean, median, min, max.")
  }
  
  if (is.null(y) && is.null(stat_type)) {
    ggplot(data, aes(x = as.factor(.data[[x]]), fill = as.factor(.data[[x]]))) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -1)
  } else {
    data_stat <- data %>% 
      group_by(!!sym(x)) %>% 
      summarize(y := ifelse(is.null(stat_type), length(!!sym(y)), get(ifelse(is.character(stat_type), stat_type, "sum"))(!!sym(y))))
    
    if (!is.null(stat_type) && is.numeric(data_stat$y)) {
      data_stat$y <- round(data_stat$y, 2)
    }
    
    ggplot(data_stat) + 
      geom_bar(aes(!!sym(x), y, fill = as.factor(!!sym(x))), 
               position = "dodge", stat = "summary", fun = ifelse(is.null(stat_type), "count", ifelse(is.character(stat_type), stat_type, "sum"))) +
      geom_text(data = data_stat, aes(label = y, x = !!sym(x), y = y), 
                position = position_dodge(width = 0.9), vjust = -0.5)
  }
}

# Example data
# barplot_stat(diamonds, "cut")
# barplot_stat(diamonds, "cut", "price", "mean")