# Function to visualize mean and sd for each level of a group across items

# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)

# Save function
visualize_means_variances <- function(data, var_names, group_var) {
  
  # Compute means and variances per group
  means_vars <- data %>%
    group_by({{ group_var }}) %>%
    summarise(across(all_of({{ var_names }}), list(mean = ~mean(.), sd = ~sd(.))))
  
  # Reshape data to long format
  means_vars_long <- means_vars %>% 
    pivot_longer(cols = -{{ group_var }}, 
                 names_to = c("variable", "stat"), 
                 names_sep = "_") %>% 
    arrange(variable)
  
  # Generate heatmap plot faceted by variable and grouped by group
  plot <- ggplot(means_vars_long, aes(x = {{ group_var }}, y = stat, fill = value)) +
    geom_tile() +
    facet_grid(variable~., scales = "free_y") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(x = rlang::as_label(rlang::enexpr(group_var)), y = NULL, fill = NULL) +
    geom_text(aes(label = round(value, 2)), color = "black", size = 4, nudge_x = 0.4)
  
  return(list(summary = means_vars, plot = plot))
}

## Example data with Group column
# set.seed(123)
# data <- data.frame(Group = rep(letters[1:3], each = 5),
#                    var1 = rnorm(15),
#                    var2 = rnorm(15),
#                    var3 = rnorm(15),
#                    var4 = rnorm(15),
#                    var5 = rnorm(15))

## Compute means and variances for var1, var2, and var3, grouped by Group, and plot the results
# results <- visualize_means_variances(data, c("var1", "var2", "var3", "var4", "var5"), Group)
# print(results$summary)
# print(results$plot)
