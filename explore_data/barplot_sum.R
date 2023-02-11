
library(ggplot2)
library(dplyr)

# Sum barplot
sum_barplot <- function(data, x, y) {
  data_sum <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := sum(!!sym(y)))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "sum") +
    geom_text(data = data_sum, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}

# Example
# sum_barplot(diamonds, "cut", "price")