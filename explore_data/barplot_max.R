
library(ggplot2)
library(dplyr)

# Max barplot
max_barplot <- function(data, x, y) {
  data_max <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := max(!!sym(y)))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "max") +
    geom_text(data = data_max, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}


# Example
# max_barplot(diamonds, "cut", "price")