## stem_fair_2026.R

# Load packages ---------------------------
library(tidyverse)
library(readxl)
library(igraph)
library(ggraph)
library(scatterpie)

# Define theming ---------------------------
palette <- c(
  "#88C0D0" = "Cultural", "#e7a042" = "Endangered Species", "#B48EAD" = "Economic", "#95c781" = "Ecological"
)
my_theme <- theme(
  axis.title = element_text(size = 16), axis.text = element_text(size = 12.5),
  legend.title = element_text(size = 16), legend.text = element_text(size = 13)
)

# Load data ---------------------------
df <- read_excel("2026 - STEM Fair/2026 11 11 - Key Species - UB STEM Fair.xlsx", sheet = 2, skip = 1) %>%
  mutate(Significance = recode(Significance, "Endangered" = "Endangered Species", "In Nature" = "Ecological"))

# Summarize results ---------------------------
n_rows <- nrow(df)
n_respondents <- length(unique(df$Respondent_ID))
# Summarize by Gender
df %>%
  group_by(Gender) %>%
  summarize(Share = paste(round(n() / n_rows * 100), "%", sep = ""))
# Summarize by Respondent Type
df %>%
  group_by(Respondent_Type) %>%
  summarize(Share = paste(round(n() / n_rows * 100), "%", sep = "")) %>%
  arrange(desc(Share))
# Summarize by Age
round(mean(df$Age, na.rm = TRUE), 1)
# Summarize by `Species or Grouping` and Significance
df %>%
  count(Species_or_Grouping, Significance) %>%
  group_by(Significance) %>%
  mutate(
    Share = paste0(round(n / n_rows * 100, 1), "%"),
    Share_of_Significance_Type = paste0(round(n / sum(n) * 100, 1), "%")
  ) %>%
  slice_max(n, n = 1) %>%
  ungroup() %>%
  arrange(Significance, desc(n))

# Make network diagram ---------------------------
significance_types <- unname(palette)
# Filter out unused data
df_filtered <- df %>%
  filter(Classification != "Non-Taxonomic Group")
# Create nodes
df_paths <- df_filtered %>%
  mutate(
    Level_0 = "",
    Level_1 = if_else(is.na(Classification), NA, paste(Level_0, Classification, sep = " / ")),
    Level_2 = if_else(is.na(Subclassification_1), NA, paste(Level_1, Subclassification_1, sep = " / ")),
    Level_3 = if_else(is.na(Subclassification_2), NA, paste(Level_2, Subclassification_2, sep = " / "))
  )

df_nodes <- df_paths %>%
  pivot_longer(starts_with("Level_"), names_to = "Classification_Level", values_to = "name") %>%
  filter(!is.na(name)) %>%
  count(name, Classification_Level, Significance) %>%
  pivot_wider(names_from = Significance, values_from = n, values_fill = 0) %>%
  mutate(
    Count = rowSums(across(all_of(significance_types))),
    Classification_Level = parse_number(Classification_Level),
    Label = str_extract(name, "[^/]+$") %>% str_trim() %>% replace_na("")
  )
# Create edges
df_edges <- map_dfr(1:3, function(i) {
  df_paths %>%
    distinct(from = .data[[paste0("Level_", i - 1)]], to = .data[[paste0("Level_", i)]]) %>%
    filter(!is.na(to))
})
# Lay out the tree
graph <- graph_from_data_frame(df_edges, vertices = df_nodes)
layout <- create_layout(graph, layout = "dendrogram", height = 3 - Classification_Level) %>%
  as_tibble() %>%
  mutate(Group = word(name, 2, sep = fixed(" / ")))
# Select node slots
df_slots_even <- layout %>%
  filter(Classification_Level == 2 | (Classification_Level == 1 & leaf)) %>%
  arrange(x) %>%
  mutate(slot = row_number() - 0.5)
slot_even <- setNames(df_slots_even$slot, df_slots_even$name)
n_slots_even <- nrow(df_slots_even)
child_spacing <- 0.9
for (parent in names(slot_even)) {
  children <- layout %>%
    filter(name %in% df_edges$to[df_edges$from == parent]) %>%
    arrange(x) %>%
    pull(name)
  offsets <- (seq_along(children) - (length(children) + 1) / 2) * child_spacing
  slot_even[children] <- slot_even[parent] + offsets
}
# Push outer-ring nodes apart at collisions
outer <- layout$name[layout$Classification_Level == 3]
outer <- outer[order(slot_even[outer])]
for (i in seq_along(outer)[-1]) {
  slot_even[outer[i]] <- max(slot_even[outer[i]], slot_even[outer[i - 1]] + child_spacing)
}
# Adjust spacing
classification_mid <- sapply(
  layout$name[layout$Classification_Level == 1],
  function(node) {
    children <- df_edges$to[df_edges$from == node]
    if (length(children) == 0) unname(slot_even[node]) else mean(range(slot_even[children]))
  }
)
classification_mid <- sort(classification_mid)
classification_base <- (seq_along(classification_mid) - 1) * n_slots_even / length(classification_mid)
slot_even[names(classification_mid)] <- classification_base + mean(classification_mid - classification_base)
# Set positions, pie sizes and labels
df_pies_even <- df_nodes %>%
  mutate(
    angle = coalesce(pi / 2 - 2 * pi * slot_even[name] / n_slots_even, 0),
    ring = c(0, 0.3, 0.76, 1.08)[Classification_Level + 1],
    x = ring * cos(angle),
    y = ring * sin(angle),
    radius = case_when(
      Classification_Level == 0 ~ 0.01,
      Classification_Level == 1 ~ 0.02 + 0.14 * sqrt(Count / max(Count)),
      TRUE ~ 0.012 + 0.1 * sqrt(Count / max(Count))
    ),
    label_x = x + (radius + 0.008) * cos(angle),
    label_y = y + (radius + 0.008) * sin(angle),
    label_hjust = 0.5 - 0.5 * cos(angle),
    label_vjust = 0.5 - 0.5 * sin(angle),
    label_y = label_y + if_else(Label == "Chickens", 0.03, 0)
  )
# Set lines between each parent and child
df_lines_even <- df_edges %>%
  inner_join(df_pies_even %>% select(from = name, x_from = x, y_from = y), by = "from") %>%
  inner_join(df_pies_even %>% select(to = name, x_to = x, y_to = y), by = "to")

# Draw and export the plot ----------------------------------------
# Register fonts
systemfonts::register_font(
  name = "Lato",
  plain = "2026 - STEM Fair/fonts/Lato-Regular.ttf", bold = "2026 - STEM Fair/fonts/Lato-Bold.ttf"
)
systemfonts::register_font(
  name = "Montserrat",
  plain = "2026 - STEM Fair/fonts/Montserrat-Bold.ttf", bold = "2026 - STEM Fair/fonts/Montserrat-Bold.ttf"
)
font_family <- "Lato"
# Draw plot
plot_network_even <- ggplot() +
  geom_segment(
    data = df_lines_even, aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
    colour = "grey75", linewidth = 0.4
  ) +
  geom_scatterpie(
    data = df_pies_even %>% filter(Classification_Level > 0), aes(x = x, y = y, r = radius),
    cols = significance_types, colour = NA
  ) +
  geom_point(
    data = df_pies_even %>% filter(Classification_Level == 0), aes(x = x, y = y),
    size = 4, colour = "grey30"
  ) +
  geom_text(
    data = df_pies_even %>% filter(Classification_Level == 0),
    aes(x = x, y = y - radius - 0.02, label = paste0("(n = ", Count, ")")),
    size = 5, fontface = "bold", vjust = 1, family = font_family
  ) +
  geom_text(
    data = df_pies_even %>% filter(Classification_Level > 0),
    aes(
      x = label_x, y = label_y, label = Label, hjust = label_hjust, vjust = label_vjust,
      size = if_else(Classification_Level == 1, 7.8, 5),
      fontface = if_else(Classification_Level == 1, "bold", "plain")
    ),
    family = font_family
  ) +
  scale_size_identity() +
  scale_fill_manual(values = setNames(names(palette), palette), name = "Reason for Importance:") +
  labs(title = "Which Belizean species are most important to you?") +
  coord_equal(clip = "off") +
  theme_void(base_family = font_family) +
  theme(
    plot.title = element_text(family = "Montserrat", size = 34, face = "bold", hjust = 0.5, margin = margin(b = 30)),
    legend.position = "bottom", legend.title = element_text(size = 28, face = "bold"),
    legend.text = element_text(size = 24, face = "bold"),
    legend.key.size = unit(2.5, "lines"), legend.margin = margin(t = 30),
    plot.margin = margin(60, 150, 20, 150)
  )
# Save plot
ggsave(
  filename = "2026 - STEM Fair/stem_fair_2026_plot_network.png",
  width = 16, height = 16, bg = "white", plot_network_even,
  device = ragg::agg_png # renders the registered fonts
)
