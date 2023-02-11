
library(ggplot2)
library(dplyr)

# Median barplot
median_barplot <- function(data, x, y) {
  data_median <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := median(!!sym(y)))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "median") +
    geom_text(data = data_median, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}

# Example
# median_barplot(diamonds, "cut", "price")