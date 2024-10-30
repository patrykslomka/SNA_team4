# Load necessary libraries
library(igraph)
library(dplyr)
library(snafun)

# Load datasets
alliances <- read.csv("Data/BACRIM2020_Alliances.csv")
rivals <- read.csv("Data/BACRIM2020_Rivals.csv")
nodes <- read.csv("Data/BACRIM2020_Nodes.csv")

# Remove duplicate alliances and rivals by creating unique pairs
alliances <- alliances %>%
  mutate(unique_pair = apply(., 1, function(row) paste(sort(c(row['Node'], row['RNode'])), collapse = "_"))) %>%
  distinct(unique_pair, .keep_all = TRUE) %>%
  select(-unique_pair)

rivals <- rivals %>%
  mutate(unique_pair = apply(., 1, function(row) paste(sort(c(row['Node'], row['RNode'])), collapse = "_"))) %>%
  distinct(unique_pair, .keep_all = TRUE) %>%
  select(-unique_pair)

#  Aggregate relationships by cartel (alliances and rivals)
# Alliances
alliance_group_df <- alliances %>%
  select(Group, RGroup, weight)
alliance_rgroup_df <- alliances %>%
  select(RGroup = Group, Group = RGroup, weight)
combined_alliance_df <- bind_rows(alliance_group_df, alliance_rgroup_df)

alliance_grouped_df <- combined_alliance_df %>%
  group_by(Group) %>%
  summarise(alliances = n(),
            total_alliance_weight = sum(weight))

# Rivals
rival_group_df <- rivals %>%
  select(Group, RGroup, weight)
rival_rgroup_df <- rivals %>%
  select(RGroup = Group, Group = RGroup, weight)
combined_rival_df <- bind_rows(rival_group_df, rival_rgroup_df)

rival_grouped_df <- combined_rival_df %>%
  group_by(Group) %>%
  summarise(rivals = n(),
            total_rival_weight = sum(weight))

# Combine alliance and rival summaries
relationship_count_df <- alliance_grouped_df %>%
  full_join(rival_grouped_df, by = "Group") %>%
  replace(is.na(.), 0)

# Display the relationship count dataframe
print(relationship_count_df)

# Add 'type' column to each dataframe
alliances$type <- "alliance"
rivals$type <- "rivalry"

# Combine alliances and rivals into a single edge list
combined_edges <- bind_rows(
  alliances %>% select(Node, RNode, weight, type),
  rivals %>% select(Node, RNode, weight, type)
) %>%
  rename(source = Node, target = RNode)

# Create an igraph object from the combined edge list
combined_graph <- graph_from_data_frame(combined_edges, directed = FALSE)

# Map Group names to node labels using the nodes dataset
V(combined_graph)$name <- nodes$Group[match(V(combined_graph)$name, nodes$Node)]

# Plot the network with improved visualization
plot(
  combined_graph,
  vertex.label = V(combined_graph)$name,       # Use cartel names as labels
  vertex.label.cex = 0.6,                      # Adjust label size for readability
  vertex.size = 10,                            # Set node size
  vertex.color = "orange",                     # Set node color
  edge.color = ifelse(E(combined_graph)$type == "alliance", "blue", "red"), # Color edges by type
  edge.width = 1.5,                            # Adjust edge width
  layout = layout_with_fr,                     # Use Fruchterman-Reingold layout for better spacing
  main = "Cartel Network with Alliances and Rivalries"
)

snafun::g_summary(combined_graph)

