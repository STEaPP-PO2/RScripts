library(ggplot2)
library(dplyr)

action_colors <- c("#770d0f", "#aa1315", "#ea3e41", "#f5a3a5", "#984EA3", "#193852", "#367cb5", "#99c0e1", "#008000", "#FFD700")

desired_order <- c("Action 1 S1 - Filtering", "Action 1 S2 - Anti-Spoofing", "Action 1 S3 - Filtering (IXP)", "Action 1 S4 - Protect L2 (IXP)", "Action 2.1 - Training", "Action 2.2 - Lab", "Action 2.3 - Technical Resources", "Action 2.4 - Hands-on")

EQU_Conformance$ACTION <- factor(EQU_Conformance$ACTION, levels = desired_order)

EQU_Conformance$Action_Type <- ifelse(EQU_Conformance$ACTION %in% c("Action 1 S1 - Filtering", "Action 1 S2 - Anti-Spoofing", "Action 1 S3 - Filtering (IXP)", "Action 1 S4 - Protect L2 (IXP)", "Action 2.1 - Training", "Action 2.2 - Lab", "Action 2.3 - Technical Resources", "Action 2.4 - Hands-on"), "Compulsory", "Recommended")

EQU_Conformance$`TOTAL PARTICIPANT CONFORMANCE %` <- as.numeric(sub("%", "", EQU_Conformance$`TOTAL PARTICIPANT CONFORMANCE %`))

bar_plot <- ggplot(EQU_Conformance, aes(x = ACTION, y = `TOTAL PARTICIPANT CONFORMANCE %`)) +
  geom_tile(data = EQU_Conformance %>% filter(Action_Type %in% c("Compulsory", "Recommended")),
            aes(fill = Action_Type), width = 1.0, height = Inf, alpha = 0.2, group = 1) +  
  geom_bar(stat = "identity", aes(fill = ACTION), position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste(`TOTAL PARTICIPANT CONFORMANCE %`, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  labs(title = "Equipment Vendor Participant Conformance % by Action",
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
