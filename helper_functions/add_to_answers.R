# Functions to save queried GPT responses into a list with the date
# See 'ask_gpt()' function, which would be run first to query
# https://platform.openai.com/docs/api-reference

# Load packages
library(httr)
library(stringr)

## Save add_to_answers function
add_to_answers <- function(char_obj) {
  today_date <- format(Sys.Date(), "%Y-%m-%d") # Get today's date in the desired format
  list_name <- paste0("gpt_answers_", today_date) # Construct the name of the list
  
  # Check if the list exists
  if (exists(list_name)) {
    # Check if a data frame for today's date exists in the list
    if (today_date %in% names(get(list_name))) {
      # Add the character object and date/time to the existing data frame
      df <- get(list_name)[[today_date]]
      df <- rbind(df, data.frame(char_obj, date_time = Sys.time()))
      assign(list_name, `[[<-`(get(list_name), today_date, df), envir = .GlobalEnv)
    } else {
      # Create a new data frame for today's date and add it to the list
      df <- data.frame(char_obj, date_time = Sys.time())
      assign(list_name, c(get(list_name), list(df)), envir = .GlobalEnv)
    }
  } else {
    # Create a new list with a data frame for today's date
    df <- data.frame(char_obj, date_time = Sys.time())
    assign(list_name, list(df), envir = .GlobalEnv)
  }
}

## Example use
# result <- ask_gpt("what is 2 + 2?")

## Add answers to a list  - constructs new object with todays date
# add_to_answers(result)

## View list
# str(`gpt_answers_2023-03-04`)

## Add a new response
# result <- ask_gpt("what is 2 - 2?")

## Append list with new data frame for answers to new question
# add_to_answers(result)

## View list
# View(`gpt_answers_2023-03-04`)
