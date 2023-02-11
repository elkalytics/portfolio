
library(ggplot2)
library(dplyr)

# Min barplot
min_barplot <- function(data, x, y) {
  data_min <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := min(!!sym(y)))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "min") +
    geom_text(data = data_min, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}


# Example
# min_barplot(diamonds, "cut", "price")