library(ggplot2)
library(dplyr)

action_colors <- c("#984EA3", "#770d0f", "#aa1315", "#ea3e41", "#f5a3a5", "#FF7F00", "#377EB8", "#dc609a", "#008000", "#FFD700")

desired_order <- c("Action 1 - Filtering", "Action 2.1 - Assist Routing Info (IRR/RPKI)", "Action 2.2 - Assist ISP Actions", "Action 2.3 - Indicate MANRS Participation", "Action 2.4 - Provide Incentives", "Action 3 - Protect Peering Platform", "Action 4 - Coordination", "Action 5 - Tools")

IXP_Conformance$ACTION <- factor(IXP_Conformance$ACTION, levels = desired_order)

IXP_Conformance$Action_Type <- ifelse(IXP_Conformance$ACTION %in% c("Action 1 - Filtering", "Action 2.1 - Assist Routing Info (IRR/RPKI)", "Action 2.2 - Assist ISP Actions", "Action 2.3 - Indicate MANRS Participation", "Action 2.4 - Provide Incentives"), "Compulsory", "Recommended")

IXP_Conformance$`TOTAL PARTICIPANT CONFORMANCE %` <- as.numeric(sub("%", "", IXP_Conformance$`TOTAL PARTICIPANT CONFORMANCE %`))

bar_plot <- ggplot(IXP_Conformance, aes(x = ACTION, y = `TOTAL PARTICIPANT CONFORMANCE %`)) +
  geom_tile(data = IXP_Conformance %>% filter(Action_Type %in% c("Compulsory", "Recommended")),
            aes(fill = Action_Type), width = 1.0, height = Inf, alpha = 0.2, group = 1) +  
  geom_bar(stat = "identity", aes(fill = ACTION), position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste(`TOTAL PARTICIPANT CONFORMANCE %`, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  labs(title = "Internet Exchange Point Participant Conformance % by Action",
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