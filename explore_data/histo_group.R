#' Histogram grouped by a variable
#'
#' This function creates a histogram grouped by a specified variable and binwidth.
#' The function can also add a density plot on top of the histogram.
#'
#' @param df A data frame
#' @param num_var A numeric variable in df to plot
#' @param group_var A grouping variable in df to group by
#' @param bin_width A numeric value specifying the width of each bin
#' @param add_density A logical value indicating whether to add a density plot on top of the histogram
#' @param title A string specifying the title of the plot
#' @param x_label A string specifying the x-axis label
#' @param y_label A string specifying the y-axis label
#' @return A ggplot object
#' @examples
#' # Example 1: Basic usage with default bin width
#' # create a sample data frame with numeric and grouping variables
#' df <- data.frame(
#'   num_var = rnorm(1000),
#'   group_var = sample(c("Group 1", "Group 2", "Group 3"), 1000, replace = TRUE)
#' )
#' # call the function with the data frame and variables
#' histo_group(df, "num_var", "group_var")
#'
#' # Example 2: Specifying bin width and adding density plot
#' # create a sample data frame with numeric and grouping variables
#' df <- data.frame(
#'   num_var = rnorm(1000),
#'   group_var = sample(c("Group 1", "Group 2", "Group 3"), 1000, replace = TRUE)
#' )
#' # call the function with the data frame and variables, specifying bin width and adding density plot
#' histo_group(df, "num_var", "group_var", bin_width = 0.1, add_density = TRUE, title = "Distribution of Numeric Variable by Group", x_label = "Numeric Variable")
#'
#' # Example 3: Changing axis labels and y-axis label
#' # create a sample data frame with numeric and grouping variables
#' df <- data.frame(
#'   num_var = rnorm(1000),
#'   group_var = sample(c("Group 1", "Group 2", "Group 3"), 1000, replace = TRUE)
#' )
#' # call the function with the data frame and variables, changing x-axis and y-axis labels
#' histo_group(df, "num_var", "group_var", x_label = "Value", y_label = "Frequency")
#' @export
histo_group <- function(df, num_var, group_var, bin_width = NULL, add_density = FALSE, title = NULL, x_label = NULL, y_label = "Count") {
  ggplot(df, aes(x = !!sym(num_var), fill = !!sym(group_var))) +
    geom_histogram(alpha = 0.5, position = "identity", binwidth = bin_width) +
    scale_fill_brewer(palette = "Set1") +
    labs(x = ifelse(is.null(x_label), num_var, x_label), y = y_label, fill = group_var, title = title) +
    theme_minimal() +
    if(add_density) {
      geom_density(alpha = 0.5, position = "identity")
    }
}