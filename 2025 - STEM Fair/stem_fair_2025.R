## stem_fair_2025.R

# Load packages ---------------------------
library(tidyverse)
library(networkD3)
library(htmlwidgets)
library(ggpubr)
library(ggmosaic)

# Load data ---------------------------
df = read.csv("2025 11 14 - Environmental Solutions - UB STEM Fair.csv")

# Create stripped pairs of concerns and solutions ---------------------------
df_pairs <- df %>%
  mutate(
    Concern_Category  = str_split(Concern_Category, ",\\s*"),
    Solution_Category = str_split(Solution_Category, ",\\s*")
  ) %>%
  unnest(Concern_Category) %>%
  unnest(Solution_Category)

# Summarize counts of each pair ---------------------------
df_flows <- df_pairs %>%
  count(Concern_Category, Solution_Category, name = "Count") %>%
  filter(Count>1)

# Plot Sankey ---------------------------
nodes <- df_flows %>%
  distinct(Concern_Category) %>%
  rename(name = Concern_Category) %>%
  bind_rows(
    df_flows %>%
      distinct(Solution_Category) %>%
      rename(name = Solution_Category)
  ) %>%
  distinct() %>%
  mutate(node_id = row_number() - 1)
links <- df_flows %>%
  left_join(nodes, by = c("Concern_Category" = "name")) %>%
  rename(source = node_id) %>%
  left_join(nodes, by = c("Solution_Category" = "name")) %>%
  rename(target = node_id) %>%
  select(source, target, value = Count)
plot_sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 70
)
saveWidget(plot_sankey, "sankey_diagram.html")

# Strip concerns and solutions ---------------------------
df_concerns <- df %>%
  mutate(Concern_Category = str_split(Concern_Category, ", ")) %>%
  unnest(Concern_Category) %>%
  group_by(Concern_Category, Respondent_Type) %>%
  summarize(Total = n()) %>%
  filter(Total >2) %>% 
  arrange(desc(Total)) %>%
  ungroup() %>%
  mutate(Concern_Category = factor(Concern_Category, levels = unique(Concern_Category), ordered = TRUE),
         Respondent_Type = factor(Respondent_Type, levels = c("UB Student", "Other Student", "UB Staff/Faculty"), ordered = TRUE))
df_solutions <- df %>%
  mutate(Solution_Category = str_split(Solution_Category, ", ")) %>%
  unnest(Solution_Category) %>%
  group_by(Solution_Category, Respondent_Type) %>%
  summarize(Total = n()) %>%
  filter(Total >3) %>% 
  arrange(desc(Total)) %>%
  ungroup() %>%
  mutate(
    Solution_Category = factor(Solution_Category, levels = unique(Solution_Category), ordered = TRUE),
    Respondent_Type = factor(Respondent_Type, levels = c("UB Student", "Other Student", "UB Staff/Faculty"), ordered = TRUE)
    )

# Create bar plots ---------------------------
plot_bar_concerns <- ggplot(df_concerns, aes(x = Concern_Category, y = Total, fill = Respondent_Type)) + 
  geom_col() +
  scale_fill_manual(values = palette) +
  theme_pubclean() +
  labs(x = "Concern", fill = "Respondent Type")
plot_bar_solutions <- ggplot(df_solutions, aes(x = Solution_Category, y = Total, fill = Respondent_Type)) + 
  geom_col() +
  scale_fill_manual(values = palette) +
  theme_pubclean() +
  labs(x = "Solution", fill = "Respondent Type")
ggsave(filename = "stem_fair_2025_plot_bar_concerns.png", width = 14, height = 6, plot_bar_concerns)
ggsave(filename = "stem_fair_2025_plot_bar_solutions.png", width = 14, height = 6, plot_bar_solutions)
