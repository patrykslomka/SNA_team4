# Load necessary libraries
library(igraph)
library(dplyr)
library(tidyr)
library(statnet)
library(snafun)

# Load the data
data <- read.csv("BACRIM-DB.csv", header=TRUE, sep=",")

# remove duplicates
data <- data[!duplicated(data),]

#A glimpse inside the data
head(data)
nrow(data)
summary(data)

# Replace missing values in character columns with "Unknown", numeric columns with 0, and keep date NAs
data <- data %>%
  mutate(across(where(is.character), ~replace_na(., "Unknown"))) %>%  # Replace NAs in character columns
  mutate(across(where(is.numeric), ~replace_na(., 0)))  

# Prepare alliance edges
allies_edges <- data %>%
  filter(!is.na(allies)) %>%
  separate_rows(allies, sep = ";") %>%
  select(group, allies) %>%
  rename(from = group, to = allies) %>%
  mutate(type = "alliance")

# Prepare rival edges
rivals_edges <- data %>%
  filter(!is.na(rivales)) %>%
  separate_rows(rivales, sep = ";") %>%
  select(group, rivales) %>%
  rename(from = group, to = rivales) %>%
  mutate(type = "rivalry")

# Combine edges
edges <- bind_rows(allies_edges, rivals_edges)

# Create igraph object
g <- graph_from_data_frame(edges, directed = FALSE)

# Add attributes to nodes if available
V(g)$criminal_activities <- data$criminal_activities[match(V(g)$name, data$group)]
V(g)$drug_trafficking <- data$drug_trafficking[match(V(g)$name, data$group)]
V(g)$armed_conflicts <- data$armed_conflicts[match(V(g)$name, data$group)]

snafun::g_summary(g)
#plot the graph
plot(g)
# Visualize the network

# Define edge colors with transparency
edge_colors <- ifelse(E(g)$type == "alliance", rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5))  # Blue for alliances, red for rivalries with transparency
edge_width <- ifelse(E(g)$type == "alliance", 0.5, 1.5)  # Thinner for alliances, thicker for rivalries

# Scale vertex size with smaller scaling factor
vertex_size <- degree(g) * 0.3 + 3

# Set labels for the top 5% highest-degree nodes only
top_nodes <- degree(g) >= quantile(degree(g), 0.95)
vertex_labels <- ifelse(top_nodes, V(g)$name, NA)  # Label only top 5% by degree

# Plot the network
plot(
  g,
  layout = layout_with_kk(g),
  vertex.label = vertex_labels,   # Show labels for high-degree nodes
  vertex.label.cex = 0.7,         # Adjust label size for readability
  vertex.label.color = "black",   # Label color
  vertex.size = vertex_size,      # Node size scaled by degree
  vertex.color = "lightblue",     # Node color
  edge.color = edge_colors,       # Edge color with transparency
  edge.width = edge_width,        # Adjust edge thickness based on type
  main = "Cartel Network: Alliances and Rivalries"
)
legend(
  "topright",
  legend = c("Alliance", "Rivalry"),
  col = c("blue", "red"),
  lwd = c(1, 1.5),
  title = "Edge Types",
  cex = 0.8,   # Smaller legend size
  bty = "n"    # No border around the legend
)
