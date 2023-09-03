library(ggplot2)
library(dplyr)

setwd("C:/Users/ahmad/Downloads")

data <- read.csv("Participant_RIR_Count.csv")

aggregated_data <- data %>%
  group_by(RIR) %>%
  summarise(Total_Count_RIR = sum(Count)) %>%
  arrange(desc(Total_Count_RIR))

data$RIR <- factor(data$RIR, levels = aggregated_data$RIR)

my_colors <- c("#9B59B6", "#30A24D", "#5D66D0", "#FF5733")

max_count <- max(aggregated_data$Total_Count_RIR)

p <- ggplot(data, aes(x = RIR, y = Count, fill = Program)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = my_colors) +  # Set custom colors
  labs(title = "Count of Participants by RIR",
       x = "RIR",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +  # Add text labels to bars
  geom_text(data = aggregated_data, 
            aes(x = RIR, y = Total_Count_RIR + max_count * 0.05, 
                label = paste0(round((Total_Count_RIR / sum(Total_Count_RIR)) * 100, 1), "%")),
            inherit.aes = FALSE)

p <- p + scale_y_continuous(limits = c(0, max_count + max_count * 0.2),
                            breaks = seq(0, max_count + max_count * 0.2, by = 25))

p <- p + theme(plot.title = element_text(hjust = 0.5))

print(p)