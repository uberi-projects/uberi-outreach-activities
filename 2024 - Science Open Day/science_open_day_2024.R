## science_open_day_2024.R

# Load packages ---------------------------
library(tidyverse)
library(wordcloud2)
library(htmlwidgets)
library(readxl)

# Load data ---------------------------
df_concerns = read_excel("2024 11 21 - Web of Concern - UB Open Science Day.xlsx")

# Define color palette ---------------------------
palette <- c(
  "#5a7494", "#b4c781", "#88C0D0", "#B48EAD", "#4C566A", "#b69efa",
  "#ce4fa4", "#e7a042", "#FB9A99", "#0d8086", "#79cc92", "#7E3793",
  "#33A02C", "#c35e68", "#eaea9a", "#9933FF", "#999933",
  "#FFCCFF", "#336600", "#999999", "#990000", "#f5f5f5ff",
  "#E22828", "coral", "#eeeec2", "violet", "#a5652a", "#d6fdfd",
  "#fcfc00", "#b69efa", "#ffecc8", "#3b3ba5"
)

# Create bar plot ---------------------------
plot_bar = ggplot(df_concerns, aes(x = Classification, fill = Classification)) + geom_bar() +
  theme_classic() +
  theme(
  legend.position = "none",
  axis.text.x = element_text(face = "bold", angle = 45, vjust = 1, hjust = 1, size = 14),
  axis.text.y = element_text(face = "bold", size = 14),
  axis.title.x = element_text(size = 18, margin = margin(30, 0, 0, 0)),
  axis.title.y = element_text(size = 18, margin = margin(0, 30, 0, 0)),
  panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5)
) +
  labs(y = "Number of Responses") +
  scale_fill_manual(values = palette)
ggsave(filename = "science_open_day_2024_plot_bar.png", width = 14, height = 6, plot_bar)

# Create word cloud ---------------------------
df_concerns_freq = df_concerns %>%
  group_by(Response) %>%
  summarise(Count = n()) %>%
  arrange(-Count)
plot_cloud = wordcloud2(df_concerns_freq, size = 1,shape = 'star', color = c(palette, palette))
saveWidget(plot_cloud, "science_open_day_2024_plot_cloud.html", selfcontained = TRUE)

