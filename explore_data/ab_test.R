#' A/B Test Function
#'
#' This function performs a two-sample t-test to compare the means of two groups: control_data and treatment_data.
#' The function calculates the mean, standard deviation, degrees of freedom, pooled standard deviation,
#' t-statistic, p-value, and conclusion based on the alpha value.
#'
#' @param control_data a numeric vector representing the control group data
#' @param treatment_data a numeric vector representing the treatment group data
#' @param alpha a numeric value representing the alpha level for the hypothesis test
#' @return a data.frame containing the mean, standard deviation, degrees of freedom,
#'         pooled standard deviation, t-statistic, p-value, and conclusion
#' @examples
#' control_data <- rnorm(100000, mean = 50, sd = 10)
#' treatment_data <- rnorm(100000, mean = 55, sd = 10)
#' results <- ab_test(control_data, treatment_data)
#' print(results)
ab_test <- function(control_data, treatment_data, alpha = 0.05) {
  
  # calculate the mean and standard deviation for each group
  control_mean <- mean(control_data)
  treatment_mean <- mean(treatment_data)
  control_sd <- sd(control_data)
  treatment_sd <- sd(treatment_data)
  
  # calculate the degrees of freedom
  control_n <- length(control_data)
  treatment_n <- length(treatment_data)
  df <- control_n + treatment_n - 2
  
  # perform t-test using t.test()
  pooled_sd <- sqrt(((control_n - 1) * control_sd^2 + (treatment_n - 1) * treatment_sd^2) / df)
  t_stat <- (treatment_mean - control_mean) / (pooled_sd * sqrt(1/control_n + 1/treatment_n))
  p_value <- 2 * pt(-abs(t_stat), df)
  
  # determine if the null hypothesis should be rejected
  conclusion <- ifelse(p_value < alpha, "Reject null hypothesis", "Fail to reject null hypothesis")
  
  # create a summary of the results
  results <- data.frame(
    control_mean = control_mean,
    treatment_mean = treatment_mean,
    control_sd = control_sd,
    treatment_sd = treatment_sd,
    pooled_sd = pooled_sd,
    t_stat = t_stat,
    p_value = p_value,
    conclusion = conclusion
  )
  
  return(results)
}