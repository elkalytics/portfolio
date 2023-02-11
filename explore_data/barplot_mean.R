
library(ggplot2)
library(dplyr)

# Mean barplot
mean_barplot <- function(data, x, y) {
  data_mean <- data %>% 
    group_by(!!sym(x)) %>% 
    summarize(!!sym(y) := round(mean(!!sym(y)), 2))
  
  ggplot(data) + 
    geom_bar(aes(!!sym(x), !!sym(y), fill = as.factor(!!sym(x))), 
             position = "dodge", stat = "summary", fun = "mean") +
    geom_text(data = data_mean, aes(label = !!sym(y), x = !!sym(x), y = !!sym(y)), 
              position = position_dodge(width = 0.9), vjust = -0.5)
}


# Example
# mean_barplot(diamonds, "cut", "price")