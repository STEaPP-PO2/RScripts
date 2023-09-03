install.packages("dplyr")
install.packages("httr")

library(dplyr)
library(httr)

setwd("C:/Users/ahmad/Downloads")

network_operators <- read.csv("NOParticipants.csv", stringsAsFactors = FALSE)

get_location <- function(asn) {
  formatted_asns <- unlist(strsplit(gsub("\\s", "", asn), ","))
  
  locations <- character(0)
  unique_countries <- character(0)
  
  for (formatted_asn in formatted_asns) {
    formatted_asn <- paste0("AS", formatted_asn)
    api_url <- paste0("https://stat.ripe.net/data/rir-geo/data.json?resource=", formatted_asn)
    
    api_response <- GET(api_url)
    api_data <- content(api_response, as = "parsed")
    
    if ("data" %in% names(api_data) && "located_resources" %in% names(api_data$data)) {
      located_resources <- api_data$data$located_resources
      if (length(located_resources) > 0) {
        location <- located_resources[[1]]$location
        if (!(location %in% unique_countries)) {
          locations <- c(locations, location)
          unique_countries <- c(unique_countries, location)
        }
      }
    }
  }
  
  if (length(locations) > 0) {
    return(paste(locations, collapse = ", "))
  } else {
    return(NA)
  }
}

network_operators$HQ.ISO2 <- sapply(network_operators$ASNs, get_location)

write.csv(network_operators, file = "NOParticipantsWithLocation.csv", row.names = FALSE)
