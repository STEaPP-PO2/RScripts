# Load data from CSV and specify column types and header
data <- NO_Trends

# Handle missing values by interpolation (replace missing values with linear interpolation)
data$`Member Count` <- na.approx(data$`Member Count`)

# Create a time series object for historical data up to 2023
historical_time_series <- ts(data$`Member Count`, start = data$Year[1], end = 2023, frequency = 1)

# Perform ETS forecasting for the years 2024 to 2028
forecast_result <- forecast::ets(historical_time_series)

# Generate forecast for the next 5 years (2024 to 2028)
forecast_values <- forecast::forecast(forecast_result, h = 5)

# Extract forecasted growth values from the forecast object
forecast_values <- as.numeric(forecast_values$mean)

# Replace the forecasted values from 2024 onwards with the actual values in the dataset
actual_values <- data$`Member Count`[data$Year >= 2024]
forecast_values[1:length(actual_values)] <- actual_values

# Visualization - Line graph showing historical data and modified forecasted growth
par(mar = c(5, 5, 4, 2) + 0.1)  # Set margin parameters for larger plot area
plot(historical_time_series, type = "l", col = "blue", lwd = 2,
     main = "Historical Data and Forecasted Growth for Network Operators",
     xlab = "Year", ylab = "Member Count",
     xlim = c(data$Year[1] - 0.5, data$Year[length(data$Year)] + 1),
     ylim = c(min(data$`Member Count`, na.rm = TRUE), max(data$`Member Count`, na.rm = TRUE, forecast_values) + 50),
     xaxs = "i", yaxs = "i")

# Add a vertical line to separate historical data from forecasted data
abline(v = 2023, col = "red", lty = 2)

# Add a legend to the plot with green line as solid (lty = 1)
legend("bottomright", legend = c("MANRS Historical Data", "Predicted Growth"), col = c("blue", "green"), lty = c(1, 1))

# Add the modified forecasted growth to the existing plot
lines(c(data$Year[length(data$Year)], seq(2024, 2028)), c(data$`Member Count`[length(data$Year)], forecast_values), col = "green", lwd = 2)

# Add the actual values for historical data (blue line) to the plot
text(data$Year, data$`Member Count`, labels = data$`Member Count`, pos = 3, col = "blue")

# Add the actual values for forecasted growth (green line) to the plot
actual_years <- data$Year[data$Year >= 2024]
actual_counts <- data$`Member Count`[data$Year >= 2024]
text(actual_years, actual_counts, labels = actual_counts, pos = 3, col = "green")