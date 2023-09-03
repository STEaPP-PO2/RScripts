library(ggplot2)
library(dplyr)

custom_palette <- c("black", "deepskyblue4", "deepskyblue", "thistle", 
                    "palevioletred1", "darkorange", "gold2", "darkgreen", 
                    "turquoise3", "magenta", "darkgray", "sienna4", "#D95B43")

socio_econ_data <- read.csv("SocioEcon_InternetUser.csv", stringsAsFactors = FALSE)

socio_econ_data$InternetPenetration <- as.numeric(as.character(socio_econ_data$InternetPenetration))
socio_econ_data$ParticipantPercentage <- as.numeric(as.character(socio_econ_data$ParticipantPercentage))

socio_econ_data <- socio_econ_data[complete.cases(socio_econ_data), ]

cor_coeff <- cor(socio_econ_data$InternetPenetration, socio_econ_data$ParticipantPercentage, method = "pearson")

scatter_plot <- ggplot(socio_econ_data, aes(x = InternetPenetration, y = ParticipantPercentage, color = UNSubRegion)) +
  geom_point(size = 4) +  
  labs(title = "Internet Penetration vs. Participant Percentage",
       x = "Internet Penetration (%)",
       y = "Participant Percentage (%)") +
  
  geom_text(aes(x = 30, y = 70, label = paste("Correlation coefficient:", round(cor_coeff, 2))),
            color = "black", hjust = 0, vjust = 1) +
  
  theme_minimal() +
  
  scale_color_manual(values = custom_palette) +
  
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),  # Remove the gray background
        panel.grid.major = element_line(color = "gray", size = 0.2),        # Add grid lines
        panel.grid.minor = element_blank())                                # Remove minor grid lines

print(scatter_plot)
