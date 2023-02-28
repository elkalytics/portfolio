# Save common scales
scale_items <- list(
  "Agreement" = c("1. Strongly disagree", "2. Disagree", "3. Neutral", "4. Agree", "5. Strongly agree"),
  "Confidence" = c("1. Not at all confident", "2. Slightly confident", "3. Moderately confident", "4. Very confident", "5. Extremely confident"),
  "Frequency" = c("1. Never", "2. Rarely", "3. Sometimes", "4. Often", "5. Always"),
  "Importance" = c("1. Not at all important", "2. Slightly important", "3. Moderately important", "4. Very important", "5. Extremely important"),
  "Likelihood" = c("1. Extremely unlikely", "2. Moderately unlikely", "3. Neither likely nor unlikely", "4. Moderately likely", "5. Extremely likely"),
  "Satisfaction" = c("1. Very dissatisfied", "2. Somewhat dissatisfied", "3. Neither satisfied nor dissatisfied", "4. Somewhat satisfied", "5. Very satisfied"),
  "Agreement7" = c("1. Strongly disagree", "2. Moderately disagree", "3. Slightly disagree", "4. Neither agree nor disagree", "5. Slightly agree", "6. Moderately agree", "7. Strongly agree"),
  "Confident9" = c("1. Not at all confident", "2. Slightly confident", "3. Somewhat confident", "4. Moderately confident", "5. Very confident", "6. Extremely confident", "7. Completely confident", "8. Almost completely confident", "9. Fairly confident"),
  "Education" = c("1. Less than high school", "2. High school diploma or equivalent", "3. Some college but no degree", "4. Associate degree", "5. Bachelor's degree", "6. Master's degree", "7. Professional degree", "8. Doctoral degree"),
  "Motivation" = c("1. Not at all motivated", "2. Slightly motivated", "3. Moderately motivated", "4. Very motivated", "5. Extremely motivated"),
  "Agreement11" = c("0. Does not apply", "1. Strongly disagree", "2. Disagree", "3. Somewhat disagree", "4. Neither agree nor disagree", "5. Somewhat agree", "6. Agree", "7. Strongly agree", "8. Completely agree", "9. Not applicable", "10. Unsure")
)

# Save function that applies common scale labels
likert_recode <- function(data, column_scale_map) {
  
  for (scale_name in names(column_scale_map)) {
    
    # Check if the specified scale name exists in the scale_items list
    if (!scale_name %in% names(scale_items)) {
      stop(paste("Invalid scale name."))
    }
    
    # Get the scale associated with the specified name
    scale <- scale_items[[scale_name]]
    
    # Set the names on the scale vector directly
    scale <- setNames(scale, 1:length(scale))
    
    # Get the columns associated with the specified scale
    columns <- column_scale_map[[scale_name]]
    
    # Loop through each column and recode it using the specified scale
    for (col in columns) {
      
      # Check if the specified column is a valid name or position number
      if (is.numeric(col) && col <= ncol(data)) {
        col_name <- names(data)[col]
      } else if (is.character(col) && col %in% names(data)) {
        col_name <- col
      } else {
        stop(paste("Invalid column name or position number."))
      }
      
      # Recode the column as an ordered factor using the mapping
      data[[col_name]] <- factor(data[[col_name]], levels = 1:length(scale), labels = scale, ordered = TRUE)
    }
  }
  
  # Return the modified data frame
  return(data)
}

## Create data frame for example 1
# example_1 <- data.frame(
#   q_agree1 = c(1, 2, 3, 4, 5),
#   q_satisfaction = c(4, 1, 2, 5, 3)
# )

## List columns
# column_scale_map <- list(
#   "Agreement" = "q_agree1",
#  "Satisfaction" = "q_satisfaction"
# )

## Run function on example 1
# example_1 <- likert_recode(example_1, column_scale_map)

## Review results
# example_1

## Create data frame for example 2
# df_2 <- data.frame(
#   q_agree1 = c(1, 2, 3, 4, 5),
#   q_agree2 = c(2, 3, 1, 5, 4),
#   q_agree3 = c(5, 4, 3, 2, 1)
# )

## List columns
# column_scale_map <- list(
#   "Agreement" = c("q_agree1", "q_agree2", "q_agree3"),
#  "Satisfaction" = c()
# )

## Run function on example 2
# example_2 <- likert_recode(df_2, column_scale_map)

## Review results
# example_2
