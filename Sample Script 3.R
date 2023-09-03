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

# Visualization - Line graph showing historical data and forecasted growth
plot(historical_time_series, type = "l", col = "blue", lwd = 2,
     main = "Historical Data and Forecasted Growth for Member Count",
     xlab = "Year", ylab = "Member Count",
     xlim = c(data$Year[1], data$Year[length(data$Year)] + 5))

# Add a vertical line to separate historical data from forecasted data
abline(v = 2024, col = "red", lty = 2)

# Add a legend to the plot
legend("topleft", legend = c("Historical Data", "Predicted Growth"), col = c("blue", "red"), lty = c(1, 2))

# Extract forecasted growth values from the forecast object
forecast_values <- as.numeric(forecast_values$mean)

# Add the forecasted growth to the existing plot
lines(seq(2024, 2028), forecast_values, col = "red", lwd = 2)

# Add a text label for the predicted growth
text(x = 2024, y = max(historical_time_series) * 0.95, labels = "Predicted Growth", col = "red")
