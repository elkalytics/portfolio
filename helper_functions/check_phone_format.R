# R function to fix phone number formatting
# Assumes desired format is (555)-555-5555

check_phone_format <- function(data, colname) {
  
  # extract the specified column from the data frame
  phone_column <- data[[colname]]
  
  # replace all non-digits with ""
  cleaned_phone <- gsub("[^0-9]", "", phone_column)
  
  # check if phone number has 7 or 10 digits
  formatted_phone <- ifelse(nchar(cleaned_phone) == 7,
                            paste0(substr(cleaned_phone, 1, 3), "-", substr(cleaned_phone, 4, 7)),
                            ifelse(nchar(cleaned_phone) == 10,
                                   paste0("(", substr(cleaned_phone, 1, 3), ")-", substr(cleaned_phone, 4, 6), "-", substr(cleaned_phone, 7, 10)),
                                   phone_column))
  
  # create number_flag column
  number_flag <- ifelse(nchar(cleaned_phone) == 7 | nchar(cleaned_phone) == 10, 0, 1)
  
  # combine the formatted phone number and number_flag as a new data frame
  output <- data.frame(phone = formatted_phone, number_flag = number_flag)
  
  # combine the new data frame with the original data frame
  result <- cbind(data, output)
  
  # return the combined data frame
  return(result)
}


## create a fake data frame
# df <- data.frame(name = c("John", "Mary", "Mike", "Sue", "Alex"),
#                  phone = c("(555) 555-5555", "6555544", "537-443-2212", "5212229983", "(555)312"))

## apply the function to the 'phone' column in the fake data frame
# check_phone_format(df, "phone")
