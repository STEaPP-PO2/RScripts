#SCRIPT1 (FULL CSV)

install.packages("wbstats")
library("wbstats")

str(wb_cachelist, max.level = 1)
new_cache <- wb_cache()

internet_stat <- wb_search("internet")
print(internet_stat)
print(n = 121 , internet_stat)

detail_results <- internet_stat[[length(internet_stat)]]
print(detail_results)

desired_indicator_id <- "IT.NET.USER"
indicator_data <- wb_data(indicator = desired_indicator_id)

print(indicator_data)
print(n = 2862 , indicator_data)

output_file <- "C:/Users/ahmad/Downloads/ITU_InternetUsersZZ.csv"

write.csv(indicator_data, file = output_file, row.names = FALSE)

cat("Data saved to", output_file, "\n")



#SCRIPT 2 (FILTERING NA VALUES AND ONLY SHOWING LATEST YEAR NUMBER)

install.packages("wbstats")
library("wbstats")

new_cache <- wb_cache()

internet_stat <- wb_search("internet")
desired_indicator_id <- "IT.NET.USER"
indicator_data <- wb_data(indicator = desired_indicator_id)

indicator_data <- indicator_data[!is.na(indicator_data$IT.NET.USER), ]
indicator_data$date <- as.Date(indicator_data$date)

latest_data <- aggregate(cbind(IT.NET.USER, date) ~ country + iso2c, indicator_data, function(x) tail(x, 1))
latest_data <- latest_data[order(latest_data$date), ]

output_file <- "C:/Users/ahmad/Downloads/ITU_InternetUsers2.csv"
write.csv(latest_data, file = output_file, row.names = FALSE)
cat("Data saved to", output_file, "\n")
