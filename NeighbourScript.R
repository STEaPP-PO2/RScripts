library(httr)
library(jsonlite)
library(dplyr)

# Set the working directory
setwd("C:/Users/ahmad/Downloads")

# Load AS numbers from the CSV file
as_data <- read.csv("ASLISTBRAZIL.csv")

# Initialize a data frame to store the results
results <- data.frame(
  AS = character(0),
  Left = numeric(0),
  Right = numeric(0),
  Unique = numeric(0),
  Uncertain = numeric(0),
  Sum = numeric(0)
)

# Loop through AS numbers and make API calls
for (i in seq_along(as_data$AS)) {
  as_number <- as_data$AS[i]
  query_url <- paste0("https://stat.ripe.net/data/asn-neighbours/data.json?resource=", as_number)
  
  response <- GET(query_url)
  if (status_code(response) == 200) {
    data <- content(response, "text")
    json_data <- fromJSON(data)
    
    neighbour_counts <- json_data$data$neighbour_counts
    left <- neighbour_counts$left
    right <- neighbour_counts$right
    unique_count <- neighbour_counts$unique
    uncertain <- neighbour_counts$uncertain
    
    sum_values <- left + right + unique_count + uncertain
    
    results <- rbind(results, data.frame(
      AS = as_number,
      Left = left,
      Right = right,
      Unique = unique_count,
      Uncertain = uncertain,
      Sum = sum_values
    ))
  } else {
    cat("Error fetching data for AS", as_number, "\n")
  }
}

# Write the results to a new CSV file
write.csv(results, "AS_RESULTS.csv", row.names = FALSE)