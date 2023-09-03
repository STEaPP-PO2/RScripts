library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(gridExtra)
library(countrycode)
library(viridis)
library(viridisLite)

setwd("C:/Users/ahmad/Downloads")

cdn_data <- read.csv("CDN_Participant_Country_Count.csv", stringsAsFactors = FALSE)
equ_data <- read.csv("EQU_Participant_Country_Count.csv", stringsAsFactors = FALSE)
no_data <- read.csv("NO_Participant_Country_Count.csv", stringsAsFactors = FALSE)
ixp_data <- read.csv("IXP_Participant_Country_Count.csv", stringsAsFactors = FALSE)

participant_data <- rbind(cdn_data, equ_data, no_data, ixp_data)

participant_data$Program <- trimws(participant_data$Program)

iso2_to_iso3 <- countrycode(sourcevar = participant_data$Country, origin = "iso2c", destination = "iso3c")
participant_data$ISO3 <- iso2_to_iso3

world_map <- ne_countries(scale = "medium", returnclass = "sf")

program_colors <- list(
  CDN = viridis(n = 5, option = "D", direction = -1),
  EQU = viridisLite::plasma(n = 5),
  NO = viridisLite::cividis(n = 5),
  IXP = viridisLite::magma(n = 5)
)

map_list <- list()

for (program in unique(participant_data$Program)) {
  program_data <- participant_data[participant_data$Program == program, ]
  
  max_count <- max(participant_data$Count, na.rm = TRUE)
  
  merged_data <- left_join(world_map, program_data, by = c("iso_a3" = "ISO3"))
  
  world_map_plot <- ggplot() +
    geom_sf(data = merged_data, color = "white", aes(fill = Count)) +
    theme_void() +
    labs(title = paste("Participant Count by Country for", program),
         caption = "Source: MANRS Participant List") +
    theme(plot.title = element_text(hjust = 0.5),   # Center the title
          plot.caption = element_text(hjust = 0.5))  # Center the caption
  
  map_list[[program]] <- world_map_plot
}

grid.arrange(grobs = map_list, ncol = 2)