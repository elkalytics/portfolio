#' Calculate customer lifetime value (LTV)
#' 
#' This function calculates the customer lifetime value (LTV) using transaction data and a discount rate.
#' 
#' @param transactions A data frame of transactions with columns "customer_id" and "amount".
#' @param discount_rate A numeric value representing the discount rate to use in the LTV calculation.
#' 
#' @return A numeric value representing the calculated LTV.
#' 
#' @examples
#' # Load transaction data into a data frame
#' transactions <- data.frame(
#'   customer_id = c(1, 1, 2, 3, 3, 3),
#'   amount = c(100, 50, 75, 200, 100, 150)
#' )
#' 
#' # Calculate LTV with a 10% discount rate
#' ltv <- calculate_ltv(transactions, 0.1)
#' 
#' # Print LTV
#' print(ltv)
#' 
#' @export
calculate_ltv <- function(transactions, discount_rate) {
  # Calculate total revenue
  total_revenue <- sum(transactions$amount)
  
  # Calculate number of customers and orders per customer
  orders_per_customer <- aggregate(transactions$amount, by=list(transactions$customer_id), FUN=length)
  num_customers <- nrow(orders_per_customer)
  
  # Calculate average order value and purchase frequency
  avg_order_value <- total_revenue / nrow(transactions)
  purchase_frequency <- nrow(transactions) / num_customers
  
  # Calculate customer value and lifespan
  customer_value <- avg_order_value * purchase_frequency
  lifespan <- 1 / (1 - purchase_frequency)
  
  # Calculate customer lifetime value
  ltv <- customer_value * lifespan / (1 + discount_rate - purchase_frequency)
  
  return(ltv)
}