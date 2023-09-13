# Define the base API URL----
base_url <- "https://kompaun.com/ekpbtwsa/api/KompaunByTarikh"

# Define the common parameters
api_key <- "TK3J2PKMCoQCSDHC8YWF"
opt_cmd <- "/J/E"

# Define the start and end dates for the loop
start_date <- as.Date("2016-01-01")
class(start_date)
end_date <- as.Date("2023-08-31")

# Initialize an empty list to store the extracted data
data_list <- list()

# Loop through the dates
for (date_numeric in seq(start_date, end_date, by = "day")) {
  # Convert the numeric date to a Date object
  date <- as.Date(date_numeric, origin = "1970-01-01")
  
  # Convert the date to the desired format (e.g., 20230101)
  date_str <- format(date, "%Y%m%d")
  
  # Create the API URL with the modified date
  api_url <- paste0(base_url, "?id=", api_key, "&docnum=", date_str, "&Optcmd=", opt_cmd)
  
  # Make the API request
  response <- GET(api_url)
  
  # Extract and store the response data
  response_content <- content(response, "text")
  
  # Parse the JSON response
  parsed_data <- fromJSON(response_content)
  
  # Store the parsed data
  data_list[[date_str]] <- parsed_data
  
  # Pause for a few seconds to avoid overwhelming the server
  Sys.sleep(3)
}

# Combine the data from data_list into a single dataframe
data_raw <- bind_rows(data_list, .id = "Date")
view(data_raw)


library(readxl)
library(writexl)
library(openxlsx)
write_xlsx(data_raw, path = "C:/Users/Admin/Downloads/31August.xlsx")

#part B----

#https://kompaun.com/ekpbtwsa/api/KompaunByTarikh?id=TK3J2PKMCoQCSDHC8YWF&docnum=20230601&Optcmd=/J/E


# response1 <- GET("https://kompaun.com/ekpbtwsa/api/KompaunByTarikh?id=TK3J2PKMCoQCSDHC8YWF&docnum=20230727&Optcmd=/J/E")
# status_code<- status_code(response1)
# 
# # Check if the request was successful (status code 200)
# if (status_code == 200) {
#   print("BERJAYA")
#   response_content1<-content(response1, as = "text")
#   parsed_data1 <- fromJSON(response_content1)
# } else {
# }
# 
# # Assuming the API response is a list of books, you can convert it to a data frame
# view(parsed_data1)
