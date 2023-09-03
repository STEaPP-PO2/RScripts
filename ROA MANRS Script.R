library(httr)
library(jsonlite)
url <- "https://api.manrs.org/roas/country/CH"
response <- VERB("GET", url, add_headers('Authorization' = 'Bearer 54607a33-8195-4298-9b5b-ccdbf67149f0'), content_type("application/octet-stream"), accept("application/json"))
content(response, "text")
print(content(response, "text"))
