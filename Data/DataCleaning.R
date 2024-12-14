# Data Exploration 
# Load the data
data <- readxl::read_excel("Data/BACRIM2020-DB.xlsx")
print(colnames(data))


# Summary of the dataset
cat("Summary of dataset:\n")
print(summary(data))

cat("\nStructure of dataset:\n")
print(str(data))

cat("\nNumber of unique cartels:", length(unique(data$grupo)), "\n")
cat("Number of alliances:", sum(!is.na(data$aliados)), "\n")
cat("Number of rivalries:", sum(!is.na(data$rivales)), "\n")
cat("Number of unique states:", length(unique(unlist(strsplit(paste(data$estado, collapse = ";"), ";")))), "\n")

# Bar plot of cartel activity by state
library(ggplot2)

# Ensure `estado` is formatted for analysis
data$estado[is.na(data$estado)] <- ""
state_counts <- table(unlist(strsplit(data$estado, ";")))

# Convert state counts to a data frame for visualization
state_counts_df <- data.frame(
  State = names(state_counts),
  Count = as.numeric(state_counts)
)

# Create the bar plot
ggplot(state_counts_df, aes(x = State, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Cartels Active in Each State", x = "State", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Network properties visualization (Degree Distribution)
library(igraph)

# Prepare the network for degree visualization
rivalry_edges <- data.frame(
  from = rep(data$grupo, times = sapply(data$rivales, function(x) length(unlist(strsplit(x, ";"))))),
  to = unlist(strsplit(paste(data$rivales, collapse = ";"), ";"))
)

# Remove NA edges
rivalry_edges <- na.omit(rivalry_edges)

# Create an igraph network
rivalry_graph <- graph_from_data_frame(rivalry_edges, directed = FALSE)

# Plot degree distribution
degree_distribution <- degree(rivalry_graph)
ggplot(data.frame(Degree = degree_distribution), aes(x = Degree)) +
  geom_histogram(bins = 20, fill = "darkorange", color = "black") +
  labs(title = "Degree Distribution of Rivalry Network", x = "Degree", y = "Frequency")

# Combine rows with the same 'grupo'
unique_groups <- unique(data$grupo)
data_combined <- data.frame(grupo = unique_groups)

# Aggregate columns for each unique 'grupo'
aggregate_column <- function(group, column, unique_values = FALSE) {
  group_data <- data[data$grupo == group, column, drop = TRUE]
  group_data <- na.omit(group_data)
  if (unique_values) {
    unique_vals <- unique(unlist(strsplit(paste(group_data, collapse = ";"), ";")))
    return(paste(unique_vals, collapse = ";"))
  } else {
    return(paste(group_data, collapse = ";"))
  }
}

binary_column <- function(group, column) {
  group_data <- data[data$grupo == group, column, drop = TRUE]
  return(as.integer(any(group_data == 1, na.rm = TRUE)))
}

# Populate the combined dataset
for (group in unique_groups) {
  data_combined[data_combined$grupo == group, "estado"] <- aggregate_column(group, "estado", TRUE)
  data_combined[data_combined$grupo == group, "aliados"] <- aggregate_column(group, "aliados", TRUE)
  data_combined[data_combined$grupo == group, "rivales"] <- aggregate_column(group, "rivales", TRUE)
  data_combined[data_combined$grupo == group, "persona"] <- aggregate_column(group, "persona", FALSE)
  data_combined[data_combined$grupo == group, "número_de_rivales"] <- binary_column(group, "número de rivales")
  data_combined[data_combined$grupo == group, "actividades_delictivas"] <- binary_column(group, "actividades_delictivas")
  data_combined[data_combined$grupo == group, "narcotrafico"] <- binary_column(group, "narcotrafico")
  data_combined[data_combined$grupo == group, "conflictos_armados"] <- binary_column(group, "conflictos_armados")
  data_combined[data_combined$grupo == group, "presencia_noviolenta"] <- binary_column(group, "presencia_noviolenta")
  data_combined[data_combined$grupo == group, "accion_guber"] <- binary_column(group, "accion_guber")
  data_combined[data_combined$grupo == group, "otros"] <- binary_column(group, "otros")
}

# Prepare edges (alliances and rivalries)
edges <- list()
row_index <- 1

for (i in seq_len(nrow(data_combined))) {
  cartel <- data_combined$grupo[i]
  
  # Process allies
  if (!is.na(data_combined$aliados[i]) && data_combined$aliados[i] != "") {
    allies <- strsplit(data_combined$aliados[i], ";")[[1]]
    for (ally in allies) {
      edges[[row_index]] <- c(from = cartel, to = ally, type = "alliance")
      row_index <- row_index + 1
    }
  }
  
  # Process rivals
  if (!is.na(data_combined$rivales[i]) && data_combined$rivales[i] != "") {
    rivals <- strsplit(data_combined$rivales[i], ";")[[1]]
    for (rival in rivals) {
      edges[[row_index]] <- c(from = cartel, to = rival, type = "rivalry")
      row_index <- row_index + 1
    }
  }
}

# Convert edges to a data frame and remove duplicates
edges_df <- do.call(rbind, edges)
edges_df <- unique(as.data.frame(edges_df))

# Sort 'from' and 'to' columns lexicographically for each row
edges_df$from_sorted <- apply(edges_df[, c("from", "to")], 1, min)
edges_df$to_sorted <- apply(edges_df[, c("from", "to")], 1, max)

# Drop duplicates based on sorted columns
unique_edges <- edges_df[!duplicated(edges_df[, c("from_sorted", "to_sorted")]), ]

# Filter edges to ensure 'from' and 'to' vertices exist in `data_combined`
valid_edges <- unique_edges[
  unique_edges$from %in% data_combined$grupo & unique_edges$to %in% data_combined$grupo, 
]

# Save valid_edges and vertex data
#write.csv(valid_edges[, c("from", "to", "type")], "valid_edges.csv", row.names = FALSE)
#write.csv(data_combined[, !colnames(data_combined) %in% c("aliados", "rivales")], "vertice_data.csv", row.names = FALSE)

# Prepare the state dependency matrix
data$estado <- ifelse(is.na(data$estado), "", data$estado)

# Create a binary state presence matrix
state_presence <- unique(data.frame(grupo = data$grupo, estado = data$estado))
state_presence <- with(state_presence, table(grupo, unlist(strsplit(estado, ";"))))

# Convert to binary
state_matrix <- as.matrix(state_presence > 0)

# Check and fix state_matrix dimensions
if (nrow(state_matrix) != ncol(state_matrix)) {
  valid_dim <- min(nrow(state_matrix), ncol(state_matrix))
  state_matrix <- state_matrix[1:valid_dim, 1:valid_dim]
}

# Ensure valid column names
if (is.null(colnames(state_matrix)) || length(colnames(state_matrix)) != ncol(state_matrix)) {
  colnames(state_matrix) <- paste0("State", seq_len(ncol(state_matrix)))
}

# Compute dependency matrix
dependency_matrix <- state_matrix %*% t(state_matrix)
diag(dependency_matrix) <- 0  # Set diagonal to zero

# Save dependency matrix
dependency_df <- as.data.frame(dependency_matrix)

# Assign row names
if (nrow(dependency_df) == length(colnames(state_matrix))) {
  rownames(dependency_df) <- colnames(state_matrix)
} else {
  stop("Number of rows in dependency_df does not match the length of state_matrix column names.")
}

# Save the dependency matrix
write.csv(dependency_df, "state_dependency_matrix.csv", row.names = TRUE)

print("Files saved successfully.")


