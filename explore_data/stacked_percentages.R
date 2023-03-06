#' Compute percentages by group and value and create a bar plot
#' 
#' This function takes an input data frame, a grouping variable, and a variable to
#' compute percentages for. It groups the data frame by both variables, counts the 
#' number of observations in each group, and computes the percentage of observations 
#' in each group for the specified variable. It then creates a bar plot of the 
#' percentages, with each bar representing a combination of the grouping variable and 
#' the specified variable. The plot includes percentage labels on each bar and is 
#' displayed on the screen. Finally, the function returns the percentages data frame.
#' 
#' @param data Input data frame.
#' @param group_var Name of the column to group by.
#' @param value_var Name of the column that contains the values to compute percentages for.
#' 
#' @return A data frame with percentages by group and value.
#' 
#' @import dplyr
#' @import ggplot2
#' 
#' @examples
#' \dontrun{
#' stacked_percentages(diamonds, cut, clarity)
#' }
# Load necessary packages
library(dplyr)
library(ggplot2)
# Define function to compute percentages and plot results
stacked_percentages <- function(data, group_var, value_var) {
  
  # Compute counts and percentages by group and value
  percentages <- data %>% # Input data frame
    group_by({{ group_var }}, {{ value_var }}) %>% # Group by two variables
    summarize(n = n()) %>% # Count number of observations in each group
    group_by({{ group_var }}, .drop = TRUE) %>% # Group by first variable only
    mutate(pct = n / sum(n) * 100) # Calculate percentages for each group and value
  
  # Create bar plot of percentages
  p <- ggplot(percentages, aes(x = {{ value_var }}, y = pct, fill = {{ group_var }})) + # Set up plot with variables and percentages
    geom_bar(position = "fill", stat = "identity", color = 'black', width = 0.9) + # Create bar plot with fill color, black outlines, and fixed width
    geom_text(aes(label = paste0(round(pct,1), "%")), # Add percentage labels with one decimal point and percent sign
              position = position_fill(vjust = 0.5), color = "black", size = 3) + # Position text in the center of each bar
    theme_bw() + # Use a black and white theme
    labs(title = "Percentages by group and value", # Set plot title and axis labels
         x = quo_name(enquo(value_var)),
         y = "Percentage")
  
  # Print plot to screen
  print(p)
  
  # Return the percentages data frame
  return(percentages)
}