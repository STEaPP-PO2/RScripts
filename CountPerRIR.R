library(ggplot2)
library(dplyr)

setwd("C:/Users/ahmad/Downloads")

data <- read.csv("Participant_RIR_Count.csv")

total_counts_RIR <- data %>%
  group_by(RIR) %>%
  summarize(total_count_RIR = sum(Count))

total_count_all <- sum(data$Count)

total_counts_RIR <- total_counts_RIR %>%
  mutate(percentage = (total_count_RIR / total_count_all) * 100)

program_colors <- c("NO" = "#23366E", "IXP" = "#D55E00", "CDN & CP" = "#009E73", "EV" = "#BF9000")

plot <- ggplot(data, aes(x = RIR, y = Count, fill = Program)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Count per Program in Each RIR") +
  xlab("RIR") +
  labs(y = "Count") +
  scale_fill_manual(values = program_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)), 
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid.major = element_line(color = "gray", size = 0.2),
        panel.grid.minor = element_line(color = "gray", size = 0.1))

plot <- plot + 
  annotate("text", x = 3, y = max(data$Count) + 60, label = "Total Contribution % Per RIR", hjust = 0.5, color = "red") +
  annotate("text", x = 1:5, y = rep(max(data$Count) + 40, 5), 
           label = paste(round(total_counts_RIR$percentage, 1), "%"), hjust = 0.5, color = "red") 

ggsave("output_graph.png", plot, width = 8, height = 6, dpi = 300, bg = "transparent")

print(plot)