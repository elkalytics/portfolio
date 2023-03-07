#' Calculate expected study duration for each company
#'
#' This function takes a data frame containing information about different companies and 
#' their panel sizes, response rates, and target demographics. It calculates the expected 
#' number of days it will take to reach the desired sample size for each company, assuming 
#' a certain proportion of male and female respondents, and a certain proportion of each 
#' generation (Boomer, GenX, Mill, GenZ).
#'
#' @param data A data frame containing information about each company and its panel size, response 
#' rate, and target demographics. The data frame must contain the following columns:
#' \itemize{
#'   \item company: A character string specifying the name of the company
#'   \item price_per_response: A numeric value specifying the price per response
#'   \item avg_daily_responses: A numeric value specifying the average number of responses per day
#'   \item total_panel_size: A numeric value specifying the total panel size for the company
#'   \item male_percent: A numeric value between 0 and 100 specifying the percentage of male respondents 
#'     targeted by the company
#'   \item female_percent: A numeric value between 0 and 100 specifying the percentage of female respondents 
#'     targeted by the company
#'   \item boomer_percent: A numeric value between 0 and 100 specifying the percentage of Boomer respondents 
#'     targeted by the company
#'   \item genx_percent: A numeric value between 0 and 100 specifying the percentage of GenX respondents 
#'     targeted by the company
#'   \item mill_percent: A numeric value between 0 and 100 specifying the percentage of Mill respondents 
#'     targeted by the company
#'   \item genz_percent: A numeric value between 0 and 100 specifying the percentage of GenZ respondents 
#'     targeted by the company
#' }
#' @param total_sample_size An integer specifying the total number of responses desired
#' @param male_prop A numeric value between 0 and 1 specifying the proportion of male respondents desired
#' @param female_prop A numeric value between 0 and 1 specifying the proportion of female respondents desired
#' @param boomer_prop A numeric value between 0 and 1 specifying the proportion of Boomer respondents desired
#' @param genx_prop A numeric value between 0 and 1 specifying the proportion of GenX respondents desired
#' @param mill_prop A numeric value between 0 and 1 specifying the proportion of Mill respondents desired
#' @param genz_prop A numeric value between 0 and 1 specifying the proportion of GenZ respondents desired
#' 
#' @return A data frame with the expected number of days it will take each company to reach the desired 
#' sample size, assuming the specified proportions of each demographic group.
#'
#' @import readxl
#' @import readr
#'
#' @examples
#' # Create fake data for six companies with varying panel sizes, response rates, and demographic targets
#' data <- data.frame(
#'   company = c("Company 1", "Company 2", "Company 3", "Company 4", "Company 5", "Company 6"),
#'   price_per_response = c(8, 6, 9, 11, 8, 7),
#'   avg_daily_responses = c(200, 300, 350, 250, 300, 200),
#'   total_panel_size = c(10000, 20000, 30000, 15000, 20000, 12000),
# Save function
calculate_study_duration <- function(data, total_sample_size, male_prop, female_prop, boomer_prop, genx_prop, mill_prop, genz_prop) {
  
  # Calculate number of male and female responses needed
  male_responses <- round(total_sample_size * male_prop)
  female_responses <- round(total_sample_size * female_prop)
  
  # Calculate the total number of responses needed
  total_responses <- male_responses + female_responses
  
  # Calculate the expected number of days to complete the study for each company
  data$male_responses <- ifelse(data$male_percent > 0, round(data$avg_daily_responses * data$male_percent/100), 0)
  data$female_responses <- ifelse(data$female_percent > 0, round(data$avg_daily_responses * data$female_percent/100), 0)
  data$boomer_responses <- ifelse(data$boomer_percent > 0, round(data$avg_daily_responses * data$boomer_percent/100), 0)
  data$genx_responses <- ifelse(data$genx_percent > 0, round(data$avg_daily_responses * data$genx_percent/100), 0)
  data$mill_responses <- ifelse(data$mill_percent > 0, round(data$avg_daily_responses * data$mill_percent/100), 0)
  data$genz_responses <- ifelse(data$genz_percent > 0, round(data$avg_daily_responses * data$genz_percent/100), 0)
  
  # Calculate the expected number of days to complete the study for each company
  data$male_days <- ifelse(data$male_responses > 0, ceiling(male_responses/data$male_responses), 0)
  data$female_days <- ifelse(data$female_responses > 0, ceiling(female_responses/data$female_responses), 0)
  data$boomer_days <- ifelse(data$boomer_responses > 0, ceiling(total_responses * boomer_prop/data$boomer_responses), 0)
  data$genx_days <- ifelse(data$genx_responses > 0, ceiling(total_responses * genx_prop/data$genx_responses), 0)
  data$mill_days <- ifelse(data$mill_responses > 0, ceiling(total_responses * mill_prop/data$mill_responses), 0)
  data$genz_days <- ifelse(data$genz_responses > 0, ceiling(total_responses * genz_prop/data$genz_responses), 0)
  
  # Calculate the total number of days to complete the study for each company
  data$total_days <- pmax(data$male_days, data$female_days, data$boomer_days, data$genx_days, data$mill_days, data$genz_days)
  
  # Return the data frame with the expected number of days for each company
  return(data)
}