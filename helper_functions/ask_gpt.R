# Functions to query/save GPT response
# https://platform.openai.com/docs/api-reference

# Save API key
api_key <- "sk-YOUR_API_KEY" 

# Load packages
library(httr)
library(stringr)

# Define ask_chatgpt function outside of gpt function
access_gpt <- function(prompt, api_key) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    )
  )
  str_trim(content(response)$choices[[1]]$message$content)
}

# Save gpt function
gpt_api <- function(api_key) {
  ask <- function(prompt) {
    access_gpt(prompt, api_key)
  }
  
  return(ask)
}

# Save API within function so the function is not re-defined with each query
ask_gpt <- gpt_api(api_key)

## Example use
# result <- ask_gpt("Can you write a poem about my cat Sam?")

## View output
# cat(result)
