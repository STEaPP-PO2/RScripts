# Load necessary libraries
library(ggplot2)
library("viridis")

# Read the dataset
data <- read.csv2("~/AVERAGE_RPKI.csv")

# Calculate Pearson correlation
correlation <- cor(data$STEM_grad_rate, data$RPKI_adoption_rate)

# Format correlation as a string
correlation_formatted <- sprintf("%.2f", correlation)

# Define a custom color palette with the specified colors
custom_palette <- c("black", "deepskyblue4", "deepskyblue", "darkorchid4", "thistle", "red3", 
                           "palevioletred1", "darkorange", "gold2", "palegreen3", "darkgreen", 
                           "turquoise3", "magenta", "darkgray", "sienna4", "#D95B43")
                           
# Create a scatter plot with custom colors for the "Sub_Region" column
ggplot(data, aes(x = STEM_grad_rate, y = RPKI_adoption_rate, color = Sub_Region)) +
  geom_point(size = 3) +  # Set the size of the dots to 3
  scale_color_manual(values = custom_palette) +  # Use custom color palette
  labs(title = paste("RPKI Adoption Rate vs. STEM Graduate Rate\nCorrelation =", correlation_formatted),
       x = "STEM Graduate Rate (%)",
       y = "RPKI Adoption Rate (%)",
       color = "Sub Region") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),     # Set title font size to 10pt
    legend.text = element_text(size = 5),    # Set legend font size to 10pt
    legend.title = element_text(size = 8),   # Set legend title font size to 10pt
    legend.position = "right"                # Place legend at the bottom
  )

