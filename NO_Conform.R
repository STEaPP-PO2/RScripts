library(ggplot2)
library(dplyr)

action_colors <- c("#E41A1C", "#377EB8", "#FF7F00", "#6e3177", "#c291ca", "#008000", "#FFD700")

desired_order <- c("Action 1 - Filtering", "Action 3 - Coordination", "Action 4 - Routing Information (IRR)", "Action 2 - Anti-Spoofing", "Action 4 - Routing Information (RPKI)")

NO_Conformance$ACTION <- factor(NO_Conformance$ACTION, levels = desired_order)

NO_Conformance$Action_Type <- ifelse(NO_Conformance$ACTION %in% c("Action 1 - Filtering", "Action 3 - Coordination", "Action 4 - Routing Information (IRR)"), "Compulsory", "Recommended")

NO_Conformance$`TOTAL PARTICIPANT CONFORMANCE %` <- as.numeric(sub("%", "", NO_Conformance$`TOTAL PARTICIPANT CONFORMANCE %`))

bar_plot <- ggplot(NO_Conformance, aes(x = ACTION, y = `TOTAL PARTICIPANT CONFORMANCE %`)) +
  geom_tile(data = NO_Conformance %>% filter(Action_Type %in% c("Compulsory", "Recommended")),
            aes(fill = Action_Type), width = 1.0, height = Inf, alpha = 0.2, group = 1) +  
  geom_bar(stat = "identity", aes(fill = ACTION), position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste(`TOTAL PARTICIPANT CONFORMANCE %`, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  labs(title = "Network Operator Participant Conformance % by Action",
       x = "Action",
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
