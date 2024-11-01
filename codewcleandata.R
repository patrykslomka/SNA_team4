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
  vertex.label = V(combined_graph)$name,       # I deleted the name of the cartels because it made the plot unreadable
  vertex.label.cex = 0.6,                      # Adjust label size for readability
  vertex.size = 10,                            # Set node size
  vertex.color = "orange",                     # Set node color
  edge.color = ifelse(E(combined_graph)$type == "alliance", "blue", "red"), # Color edges by type
  edge.width = 1.5,                            # Adjust edge width
  layout = layout_with_fr,                     # Use Fruchterman-Reingold layout for better spacing
  main = "Cartel Network with Alliances and Rivalries"
)

# Display a summary of the network
snafun::g_summary(combined_graph)

# Calculate and plot centralities
snafun::plot_centralities(
  combined_graph,
  measures = c("betweenness", "closeness", "degree", "eccentricity"), 
  directed = TRUE,
  mode = "all",
  k = 3,
  rescaled = FALSE
)

# Calculate degree distribution
degree_distribution <- snafun::g_degree_distribution(combined_graph, 
                                                     mode = "all", 
                                                     type = "count")
# Create a frequency plot with lines for degree distribution
plot(1:length(degree_distribution), degree_distribution, type = "h", lwd = 2, 
     main = "Degree Distribution", xlab = "Degree", ylab = "Frequency")


# Improved visualization of the cartel network with alliances and rivalries
# Map Group names to node labels using the nodes dataset
V(combined_graph)$name <- nodes$Group[match(V(combined_graph)$name, nodes$Node)]

# Apply community detection using the Louvain method
communities <- cluster_louvain(combined_graph)

# Assign each node a unique color based on its community membership
community_colors <- rainbow(length(unique(membership(communities))))
V(combined_graph)$color <- community_colors[membership(communities)]

# Adjust node sizes based on degree
V(combined_graph)$size <- igraph::degree(combined_graph, mode = "all") * 1.5 + 5

# Adjust edge width based on weight and set colors with transparency
E(combined_graph)$width <- E(combined_graph)$weight / 2
E(combined_graph)$color <- ifelse(E(combined_graph)$type == "alliance", adjustcolor("blue", alpha.f = 0.5), adjustcolor("red", alpha.f = 0.5))
E(combined_graph)$curved <- 0.1  # Reduce edge curvature

# Set background color
par(bg = "lightgray")

# Plot the network with improved visualization
plot(
  combined_graph,
  vertex.label.dist = 0.5,                # Offset labels slightly from nodes
  vertex.label = ifelse(degree(combined_graph) > 8, V(combined_graph)$name, NA),  # Only label high-degree nodes
  vertex.size = V(combined_graph)$size,                                            # Set node size by degree
  layout = layout_with_fr(combined_graph, niter = 10000, area = 30 * vcount(combined_graph)^2),                                                         # Use Fruchterman-Reingold layout
  main = "Improved Cartel Network with Alliances and Rivalries"
)

# Add a legend for community colors
legend(
  "topright",
  legend = paste("Community", 1:length(unique(membership(communities)))),
  col = community_colors,
  pch = 19,           # Use filled circles in legend
  title = "Cartel Communities",
  cex = 0.8           # Adjust legend size for readability
)

# Add a legend for edge colors
legend(
  "bottomright",
  legend = c("Alliance", "Rivalry"),
  col = c("blue", "red"),
  lwd = 2,             # Line width for edge type in legend
  title = "Relationship Type",
  cex = 0.8            # Adjust legend size for readability
)

