install.packages("dplyr")
install.packages("httr")

library(dplyr)
library(httr)

setwd("C:/Users/ahmad/Downloads")

network_operators <- read.csv("NOParticipants.csv", stringsAsFactors = FALSE)

get_rir <- function(asn) {
  formatted_asns <- unlist(strsplit(gsub("\\s", "", asn), ","))
  
  rirs <- character(0)
  
  for (formatted_asn in formatted_asns) {
    formatted_asn <- paste0("AS", formatted_asn)
    api_url <- paste0("https://stat.ripe.net/data/rir/data.json?resource=", formatted_asn)
    
    api_response <- GET(api_url)
    api_data <- content(api_response, as = "parsed")
    
    if ("data" %in% names(api_data) && "rirs" %in% names(api_data$data)) {
      if (length(api_data$data$rirs) > 0) {
        rir <- api_data$data$rirs[[1]]$rir
        rirs <- c(rirs, rir)
      }
    }
  }
  
  unique_rirs <- unique(rirs)
  
  if (length(unique_rirs) > 0) {
    return(paste(unique_rirs, collapse = ", "))
  } else {
    return(NA)
  }
}

network_operators$RIR <- sapply(network_operators$ASNs, get_rir)

write.csv(network_operators, file = "NOParticipantsWithRIRs.csv", row.names = FALSE)
