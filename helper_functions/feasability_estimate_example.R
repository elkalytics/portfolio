# Cost and duration estimator

# Save function
feasability_estimate <- function(data, male_pct, female_pct, boomer_pct, genx_pct, mill_pct, genz_pct, sample_size, missing_rate) {
  # calculate the number of people needed in each demographic group
  total_sample_size <- sample_size / (1 - missing_rate)
  num_males <- round(total_sample_size * male_pct / 100)
  num_females <- round(total_sample_size * female_pct / 100)
  num_boomers <- round(total_sample_size * boomer_pct / 100)
  num_genx <- round(total_sample_size * genx_pct / 100)
  num_mill <- round(total_sample_size * mill_pct / 100)
  num_genz <- round(total_sample_size * genz_pct / 100)
  
  # calculate the number of days and cost for each company
  data$sample_size <- round(data$daily_responses * (1 - missing_rate))
  data$male_responses <- round(data$sample_size * data$male_percent / 100)
  data$female_responses <- round(data$sample_size * data$female_percent / 100)
  data$boomer_responses <- round(data$sample_size * data$boomer_percent / 100)
  data$genx_responses <- round(data$sample_size * data$genx_percent / 100)
  data$mill_responses <- round(data$sample_size * data$mill_percent / 100)
  data$genz_responses <- round(data$sample_size * data$genz_percent / 100)
  data$male_days <- ceiling(num_males / data$male_responses)
  data$female_days <- ceiling(num_females / data$female_responses)
  data$boomer_days <- ceiling(num_boomers / data$boomer_responses)
  data$genx_days <- ceiling(num_genx / data$genx_responses)
  data$mill_days <- ceiling(num_mill / data$mill_responses)
  data$genz_days <- ceiling(num_genz / data$genz_responses)
  data$total_days <- pmax(data$male_days, data$female_days, data$boomer_days, data$genx_days, data$mill_days, data$genz_days)
  data$total_cost <- data$total_days * data$price_per_response * data$daily_responses
  data <- data[, c("company", "total_days", "total_cost")]
  return(data)
}

# Construct fake data
data <- data.frame(
  company = c("Company 1", "Company 2", "Company 3", "Company 4", "Company 5", "Company 6"),
  price_per_response = c(7, 6, 14, 11, 8, 5),
  daily_responses = c(20, 30, 35, 49, 33, 36),
  male_percent = c(52, 45, 40, 41, 46, 61),
  female_percent = c(48, 55, 60, 59, 54, 39),
  boomer_percent = c(28, 31, 25, 29, 26, 35),
  genx_percent = c(27, 30, 28, 29, 20, 28),
  mill_percent = c(15, 11, 20, 10, 15, 15),
  genz_percent = c(30, 28, 27, 32, 39, 22)
)

# View data
head(data)

# Estimate feasability
feasability_estimate(data, 
                     male_pct = 50, 
                     female_pct = 50,
                     boomer_pct = 20,
                     genx_pct = 20, 
                     mill_pct = 30, 
                     genz_pct = 10, 
                     sample_size = 1000, 
                     missing_rate = 0.05) -> results

# View results
head(results)
