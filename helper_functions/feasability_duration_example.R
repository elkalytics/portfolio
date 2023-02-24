# Another approach to study feasibility. This approach focuses on estimating time
# Made adjustments to handle proportions of 0 and expanded output to see time for each group

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


# Construct fake data
data <- data.frame(
  company = c("Company 1", "Company 2", "Company 3", "Company 4", "Company 5", "Company 6"),
  price_per_response = c(8, 6, 9, 11, 8, 7),
  avg_daily_responses = c(200, 300, 350, 250, 300, 200),
  total_panel_size = c(10000, 20000, 30000, 15000, 20000, 12000),
  male_percent = c(0, 45, 40, 41, 46, 61),
  female_percent = c(100, 55, 60, 59, 54, 39),
  boomer_percent = c(28, 31, 25, 29, 26, 35),
  genx_percent = c(27, 30, 28, 29, 20, 28),
  mill_percent = c(15, 11, 20, 10, 15, 15),
  genz_percent = c(30, 28, 27, 32, 39, 22)
)

# Example use 1
calculate_study_duration(data, 
                         1000, 0, 1, 0.2, 0.3, 0.2, 0.3) -> results_expanded

# Example use 2
calculate_study_duration(data, 
                         2500, .5, .5, 0.5, 0.5, 0.0, 0.0) -> results_expanded_2