# Load the libraries
library(ggplot2)
library(forecast)

# Assuming you have the data in a data frame named 'NO_Trends'

# Create the time series object
ts_data <- ts(NO_Trends$`Member Count`, start = 2014, frequency = 1)

# Forecast the participation count
model <- auto.arima(ts_data)
forecast <- forecast(model, h = 5)  # Forecast horizon set to 5 periods

# Create a new time series object that includes both historical and forecasted data
ts_combined <- ts(c(ts_data, forecast$mean), start = start(ts_data))

# Create a data frame for plotting
forecast_data <- data.frame(Year = time(ts_combined),
                            `Member Count` = c(coredata(ts_data), forecast$mean),
                            Forecast = c(rep("Historical Data", length(ts_data)), rep("Forecast", 5)))

# Plot the forecast with the legend for historical data
ggplot(forecast_data, aes(x = Year, y = Member.Count, color = Forecast)) +
  geom_line() +
  labs(title = "MANRS Participation Trend",
       x = "Year",
       y = "Member Count",
       color = "Legend Title") +
  theme_minimal()
