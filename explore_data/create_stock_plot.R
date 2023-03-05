#' Create a combined plot of revenue boxplot by year and cumulative sales over time
#' 
#' User specifies data as well as columns for year and revenue.
#'
#' @param data A data.frame containing the revenue data.
#' @param year_var A string specifying the name of the column in \code{data} containing the years.
#' @param revenue_var A string specifying the name of the column in \code{data} containing the revenue values.
#'
#' @return A ggplot object of the combined plot.
#'
#' @import ggplot2
#' @import ggpubr
#' @import ggpmisc
#'
#' @examples
#' # Create a fake data set
#' revenue_data <- data.frame(year = c(2018, 2018, 2019, 2019, 2020, 2020, 2021, 2021),
#'                            sales = c(50, 150, 100, 200, 75, 75, 200, 100))
#' # Apply function
#' create_stock_plot(data = revenue_data, year_var = "year", revenue_var = "sales")
#' 
create_stock_plot <- function(data, year_var, revenue_var) {
  library(ggplot2)
  library(ggpubr)
  library(ggpmisc)
  
  # Convert data to a data frame
  data_df <- data.frame(data)
  
  # Calculate stock values
  stock_values <- cumsum(data_df[[revenue_var]])
  
  # Add stock values to data frame
  data_df$stock_value <- stock_values
  
  # Create boxplot
  boxplot <- ggplot(data_df, aes(x = as.factor(.data[[year_var]]), y = .data[[revenue_var]])) +
    geom_boxplot(fill = "white", color = "gray50") +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "Year", y = "Revenue", title = "Revenue Boxplot by Year") +
    theme_classic()
  
  # Create stock plot with trend line and formula
  stock_plot <- ggplot(data_df, aes(x = .data[[year_var]], y = stock_value)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "orange") +
    stat_poly_eq(formula = y ~ x, aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")), 
                 parse = TRUE, size = 5, color = "black") +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "Year", y = "Cumulative Revenue", title = "Stock Plot of Cumulative Revenue by Year") +
    theme_classic()
  
  # Combine boxplot and stock plot
  combined_plot <- ggarrange(boxplot, stock_plot, ncol = 1, heights = c(0.4, 0.6))
  
  return(combined_plot)
}