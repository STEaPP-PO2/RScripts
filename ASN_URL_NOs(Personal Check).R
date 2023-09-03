install.packages("dplyr")

library(dplyr)

network_operators <- read.csv("C:/Users/ahmad/Downloads/NOParticipants.csv", stringsAsFactors = FALSE)

formatted_urls <- character(0)

for (row in 1:nrow(network_operators)) {
  operator_asns <- strsplit(network_operators$ASNs[row], ",")[[1]]
  
  for (operator_asn in operator_asns) {
    formatted_asn <- gsub("\\s", "", operator_asn)
    formatted_asn <- paste0("AS", formatted_asn)
    api_url <- paste0("https://stat.ripe.net/data/rir/data.json?resource=", formatted_asn)
    
    formatted_urls <- c(formatted_urls, api_url)
  }
}

formatted_urls_df <- data.frame(FormattedURLs = formatted_urls)
write.csv(formatted_urls_df, file = "FormattedURLs.csv", row.names = FALSE)