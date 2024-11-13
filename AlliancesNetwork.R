# Load necessary libraries
library(igraph)
library(ggplot2)
library(ggraph)
library(snafun)

# Load the data
edges <- read.csv("Valid_edges.csv")
vertices <- read.csv("vertice_data.csv")

# Filter only alliance relationships
alliance_edges <- subset(edges, type == "alliance")

# Create the graph object
g_alliances <- graph_from_data_frame(d = alliance_edges, directed = FALSE)

# Plot the network using ggraph
ggraph(g_alliances, layout = "fr") +
  geom_edge_link(color = "gray", width = 0.8) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  ggtitle("Alliance Network of Groups")


snafun::g_summary(g_alliances)

# Calculate centrality measures and other node metrics
degree_centrality <- degree(g_alliances)
betweenness_centrality <- betweenness(g_alliances, normalized = TRUE)
closeness_centrality <- closeness(g_alliances, normalized = TRUE)
eigenvector_centrality <- eigen_centrality(g_alliances)$vector
transitivity_values <- transitivity(g_alliances, type = "local", isolates = "zero")


# Create a data frame with all node attributes
node_attributes <- data.frame(
  name = V(g_alliances)$name,
  degree_centrality = degree_centrality,
  betweenness_centrality = betweenness_centrality,
  closeness_centrality = closeness_centrality,
  eigenvector_centrality = eigenvector_centrality,
  transitivity = transitivity_values
)

# Display the data frame
print(node_attributes)

# Save the node attributes as a CSV file
write.csv(node_attributes, "alliance_network_node_attributes.csv", row.names = FALSE)
