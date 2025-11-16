## stem_fair_2025.R

# Load packages ---------------------------
library(tidyverse)
library(networkD3)
library(htmlwidgets)

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
