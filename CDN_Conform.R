library(ggplot2)
library(dplyr)

action_colors <- c("#aa1315", "#ea3e41", "#f5a3a5", "#377EB8", "#984EA3", "#FF7F00", "#ffb870", "#454fb0", "#dc609a", "#008000", "#FFD700")

desired_order <- c("Action 1a - Filtering (Baseline)", "Action 1b - RPKI", "Action 1c - AS-SET", "Action 2 - Anti-Spoofing", "Action 3 - Coordination", "Action 4a - Routing Information (IRR)", "Action 4b - Routing Information (RPKI)", "Action 5 - MANRS Adoption", "Action 6 - Tools")

CDN_Conformance$ACTION <- factor(CDN_Conformance$ACTION, levels = desired_order)

CDN_Conformance$Action_Type <- ifelse(CDN_Conformance$ACTION %in% c("Action 1a - Filtering (Baseline)", "Action 1b - RPKI", "Action 1c - AS-SET", "Action 2 - Anti-Spoofing", "Action 3 - Coordination", "Action 4a - Routing Information (IRR)", "Action 4b - Routing Information (RPKI)", "Action 5 - MANRS Adoption"), "Compulsory", "Recommended")

CDN_Conformance$`TOTAL PARTICIPANT CONFORMANCE %` <- as.numeric(sub("%", "", CDN_Conformance$`TOTAL PARTICIPANT CONFORMANCE %`))

bar_plot <- ggplot(CDN_Conformance, aes(x = ACTION, y = `TOTAL PARTICIPANT CONFORMANCE %`)) +
  geom_tile(data = CDN_Conformance %>% filter(Action_Type %in% c("Compulsory", "Recommended")),
            aes(fill = Action_Type), width = 1.0, height = Inf, alpha = 0.2, group = 1) +  
  geom_bar(stat = "identity", aes(fill = ACTION), position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste(`TOTAL PARTICIPANT CONFORMANCE %`, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  labs(title = "Content Delivery Network & Cloud Participant Conformance % by Action",
       x = "",  # Remove x-axis label
       y = "Total Participant Conformance (%)",
       fill = NULL) +  
  scale_fill_manual(values = action_colors) +  
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 10), expand = c(0.02, 0)) +  
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", 
        legend.title = element_blank())

print(bar_plot)
