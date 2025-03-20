library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(lubridate)
library(stringr)
library(data.table)
library(R.utils)
library(readxl)
library(httr)
library(jsonlite)
library(openxlsx)

file_path <- "C:\\Users\\Jakob German\\Downloads\\codelist_Koulutusluokitus.xlsx"

# Read the third sheet (sheet index 3)
dict_df <- read_excel(file_path, sheet = 3)
glimpse(dict_df)

dict_df <- dict_df %>% 
  select(`CODEVALUE`, `PREFLABEL_FI`)

nrow(dict_df) # 587 profession codes
View(dict_df)
fwrite(dict_df, "C:\\Users\\Jakob German\\Downloads\\MD_profession_dictionary.csv")


# Define a function to translate text using Google Translate API
google_translate <- function(text, source = "fi", target = "en") {
  base_url <- "https://translate.googleapis.com/translate_a/single"
  
  # Prepare the query parameters
  params <- list(
    client = "gtx",
    sl = source,
    tl = target,
    dt = "t",
    q = text
  )
  
  # Send the GET request with a 60-second timeout
  response <- GET(url = base_url, query = params, timeout(60))
  
  if (status_code(response) != 200) {
    warning("Request failed with status: ", status_code(response))
    return(NA)
  }
  
  # Parse the response text into R object
  result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  # The translated text is in the first element of the result;
  # We concatenate all parts to get the complete translation.
  translated <- paste(sapply(result[[1]], function(x) x[[1]]), collapse = "")
  return(translated)
}

# Test: Translate a single piece of Finnish text
sample_text <- "Opetus on avain tulevaisuuteen." # "Education is the key to the future."
google_translate(sample_text)

dict_df$PREFLABEL_EN <- sapply(dict_df$PREFLABEL_FI, google_translate)
fwrite(dict_df, "C:\\Users\\Jakob German\\Downloads\\MD_profession_dictionary_EN.csv")
# Write the dataframe to an Excel file
write.xlsx(dict_df, "C:\\Users\\Jakob German\\Downloads\\MD_profession_dictionary_EN.xlsx")

