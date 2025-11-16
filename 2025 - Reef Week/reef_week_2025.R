## reef_week_2025.R

# Load packages ---------------------------
library(ggplot2)
library(readr)
library(dplyr)
library(readxl)

# Load data ---------------------------
data <- read_excel("2025 - Web of Concern - Belize Reef Week.xlsx")

# Define color palette ---------------------------
custom_colors <- c(
  "Biodiversity Loss" = "#58748A",
  "Climate Change"    = "#B5C88C",
  "Governance"        = "#85B6C9",
  "Habitat Loss"      = "#B892A4",
  "Over Exploitation" = "#4E5563",
  "Pollution"         = "#B9A2F2"
)

# Summarize counts by classification ---------------------------
counts <- data %>%
  count(Classification) 

# Create bar plot ---------------------------
plot_bar <- ggplot(counts, aes(x = Classification, y = n, fill = Classification)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = custom_colors) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "gray30"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "gray80"), 
    panel.grid.minor.y = element_blank(),               
    axis.line = element_line(color = "black", linewidth = 0.8) 
  ) +
  labs(
    x = "Classification",
    y = "Number of Responses"
  )
ggsave(filename = "reef_week_2025_plot_bar.png", width = 14, height = 6, plot_bar)
