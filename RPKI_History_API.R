library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

setwd("C:/Users/ahmad/Downloads")

iso2_data <- read.csv("worldiso2.csv")

base_url <- "https://stat.ripe.net/data/rpki-history/data.json"

result_df <- data.frame()

for (iso2 in iso2_data$Country) {
  api_url <- paste0(base_url, "?resource=", iso2, "&delegated=true")
  
  response <- GET(api_url)
  
  if (status_code(response) == 200) {
    response_data <- content(response, "text")
    parsed_data <- fromJSON(response_data)
    
    if ("data" %in% names(parsed_data) && "timeseries" %in% names(parsed_data$data) && length(parsed_data$data$timeseries) > 0) {
      
      latest_timeseries <- tail(parsed_data$data$timeseries, 1)
      
      if ("delegated" %in% names(latest_timeseries) && "space" %in% names(latest_timeseries$delegated)) {
        space_count <- latest_timeseries$delegated$space$count
        covered_by_rpki_count <- latest_timeseries$delegated$space$covered_by_rpki$count
        
        iso2_result <- data.frame(iso2 = iso2, space_count = space_count, covered_by_rpki_count = covered_by_rpki_count)
        
        result_df <- bind_rows(result_df, iso2_result)
      } else {
        cat("Error: Data structure in response is not as expected for ISO2 =", iso2, "\n")
      }
    } else {
      cat("Error: No timeseries data found for ISO2 =", iso2, "\n")
    }
  } else {
    cat("Error fetching data for ISO2 =", iso2, "\n")
  }
}

write.csv(result_df, "RPKI_RESULTS.csv", row.names = FALSE)

cat("Results saved to AS_RESULTS.csv\n")