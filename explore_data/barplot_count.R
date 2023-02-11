
library(ggplot2)

# Count barplot function
count_barplot <- function(data, field) {
  ggplot(data, aes(x = as.factor(.data[[field]]), fill = as.factor(.data[[field]]))) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -1)
}

# Example
# count_barplot(diamonds, "cut")
