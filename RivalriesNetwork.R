# Load necessary libraries
library(igraph)
library(dplyr)
library(snafun)
library(intergraph)
library(ergm)
# Load datasets
rivals <- read.csv("Data/BACRIM2020_Rivals.csv")
nodes <- read.csv("Data/BACRIM2020_Nodes.csv")
alliances <- read.csv("Data/BACRIM2020_Alliances.csv")

# Remove duplicate rivalries by creating unique pairs
rivals <- rivals %>%
  mutate(unique_pair = apply(., 1, function(row) paste(sort(c(row['Node'], row['RNode'])), collapse = "_"))) %>%
  distinct(unique_pair, .keep_all = TRUE) %>%
  select(-unique_pair)

# Aggregate rivalries by cartel
rival_group_df <- rivals %>%
  select(Group, RGroup, weight)
rival_rgroup_df <- rivals %>%
  select(RGroup = Group, Group = RGroup, weight)
combined_rival_df <- bind_rows(rival_group_df, rival_rgroup_df)

rival_grouped_df <- combined_rival_df %>%
  group_by(Group) %>%
  summarise(rivals = n(),
            total_rival_weight = sum(weight))

# Display the rivalry count dataframe
print(rival_grouped_df)

# Create an igraph object for rivalries only
rivalry_edges <- rivals %>%
  select(Node, RNode) %>%
  rename(source = Node, target = RNode)

rivalry_graph <- graph_from_data_frame(rivalry_edges, directed = FALSE)

# Map Group names to node labels using the nodes dataset
V(rivalry_graph)$name <- nodes$Group[match(V(rivalry_graph)$name, nodes$Node)]

# Plot the network with improved visualization
plot(
  rivalry_graph,
  vertex.label = V(rivalry_graph)$name,
  vertex.label.cex = 0.6,
  vertex.size = 10,
  vertex.color = "orange",
  edge.color = "red",  # Set edge color for rivalries
  edge.width = 1.5,
  layout = layout_with_fr,
  main = "Cartel Network with Rivalries"
)

# Display a summary of the network
summary(rivalry_graph)
snafun::g_summary(rivalry_graph)

# Calculate and plot centralities for the rivalry network
snafun::plot_centralities(
  rivalry_graph,
  measures = c("betweenness", "closeness", "degree", "eccentricity"),
  directed = TRUE,
  mode = "all",
  k = 3,
  rescaled = FALSE
)

# Degree distribution
degree_distribution <- snafun::g_degree_distribution(rivalry_graph, 
                                                     mode = "all", 
                                                     type = "count")
plot(1:length(degree_distribution), degree_distribution, type = "h", lwd = 2, 
     main = "Degree Distribution", xlab = "Degree", ylab = "Frequency")

# Community detection and improved visualization
communities <- cluster_louvain(rivalry_graph)

# Assign each node a unique color based on its community membership
community_colors <- rainbow(length(unique(membership(communities))))
V(rivalry_graph)$color <- community_colors[membership(communities)]
V(rivalry_graph)$size <- igraph::degree(rivalry_graph, mode = "all") * 0.5 + 5

# Set edge color with transparency
E(rivalry_graph)$color <- adjustcolor("red", alpha.f = 0.5)
E(rivalry_graph)$curved <- 0.1

# Set background color and label adjustments
par(bg = "lightgray")
V(rivalry_graph)$label.color <- "black"

# Plot the rivalry network with community colors
plot(
  rivalry_graph,
  vertex.label = ifelse(igraph::degree(rivalry_graph) > 10, V(rivalry_graph)$name, NA),
  vertex.label.cex = 0.8,
  vertex.label.dist = 0.5,
  vertex.size = V(rivalry_graph)$size,
  layout = layout_with_fr(rivalry_graph, niter = 10000, area = 30 * vcount(rivalry_graph)^2),
  main = "Improved Cartel Network with Rivalries"
)

# Add a legend for community colors
legend(
  "topright",
  legend = paste("Community", 1:length(unique(membership(communities)))),
  col = community_colors,
  pch = 19,
  title = "Cartel Communities",
  cex = 0.8
)

# Add a legend for rivalry edges
legend(
  "bottomright",
  legend = "Rivalry",
  col = "red",
  lwd = 2,
  title = "Relationship Type",
  cex = 0.8
)
#------------
# Remove duplicate alliances by creating unique pairs
alliances <- alliances %>%
  mutate(unique_pair = apply(., 1, function(row) paste(sort(c(row['Node'], row['RNode'])), collapse = "_"))) %>%
  distinct(unique_pair, .keep_all = TRUE) %>%
  select(-unique_pair)

# Aggregate alliances by cartel
alliance_group_df <- alliances %>%
  select(Group, RGroup, weight)
alliance_rgroup_df <- alliances %>%
  select(RGroup = Group, Group = RGroup, weight)
combined_alliance_df <- bind_rows(alliance_group_df, alliance_rgroup_df)

alliance_grouped_df <- combined_alliance_df %>%
  group_by(Group) %>%
  summarise(alliances = n(),
            total_alliance_weight = sum(weight))

# Display the alliance count dataframe
print(alliance_grouped_df)

# Create an igraph object for alliances only
alliance_edges <- alliances %>%
  select(Node, RNode, weight) %>%
  rename(source = Node, target = RNode)

alliance_graph <- graph_from_data_frame(alliance_edges, directed = FALSE)

# Map Group names to node labels using the nodes dataset
V(alliance_graph)$name <- nodes$Group[match(V(alliance_graph)$name, nodes$Node)]


# Convert the igraph rivalry graph to a network object for ERGM modeling
rivalry_net <- asNetwork(rivalry_graph)

# Convert the alliance igraph object to a network object for use as an edge covariate
alliance_net <- asNetwork(alliance_graph)
alliance_cov <- as.matrix.network(alliance_net)
print(network.size(rivalry_net))
print(network.size(alliance_net))

# Fit the ERGM model on the rivalry network with alliance data as a covariate
model <- ergm(
  rivalry_net ~ edges + 
    gwesp(0.5, fixed = TRUE) +     # Geometrically Weighted Edgewise Shared Partners for triadic closure
    triangle +                     # Triadic closure to capture "enemy of my enemy" effect
    degree(1) +                    # Controls for popular cartels with more rivalries
    edgecov(alliance_cov),         # Edge covariate to test influence of alliances on rivalry formation
  control = control.ergm(MCMC.samplesize = 5000, MCMC.burnin = 1000)
)

# Display the model summary
summary(model)



model <- ergm(rivalry_network ~ edges)  # Define the ERGM model on the network object
# Define the ERGM model on the network object
model <- ergm(rivalry_network ~ edges + 
                gwesp(decay = 0.5, fixed = TRUE) +  # Triadic closure for shared enemies
                edgecov(rivalry_network) +  # Influence of shared rivalry on alliances
                nodematch("region") +  # Optional: if region is relevant
                nodecov("activity_count"))  # Optional: if diversification of criminal activities matters

summary(model)


model2 <- ergm(rivalry_network ~ edges + 
                gwesp(decay = 0.5, fixed = TRUE))

summary(model2)


model3 <- ergm(rivalry_network ~ edges + 
                gwesp(decay = 0.5, fixed = TRUE) +  # Triadic closure for shared enemies
                edgecov(rivalry_network))  # Influence of shared rivalry on alliances
