# Load the data
data <- readxl::read_excel("Data/BACRIM2020-DB.xlsx")

# Ensure the 'estado' column contains the states in a format that can be split
data$estado[is.na(data$estado)] <- ""  # Fill any NA with an empty string

# Extract unique combinations of 'grupo' and 'estado'
states_expanded <- data.frame(grupo = data$grupo, estado = data$estado)
states_expanded <- unique(states_expanded)

# One-hot encode the 'estado' column
unique_states <- unique(unlist(strsplit(paste(states_expanded$estado, collapse = ";"), ";")))
state_matrix <- matrix(0, nrow = nrow(states_expanded), ncol = length(unique_states),
                       dimnames = list(states_expanded$grupo, unique_states))

for (i in seq_len(nrow(states_expanded))) {
  states <- unlist(strsplit(states_expanded$estado[i], ";"))
  state_matrix[states_expanded$grupo[i], states] <- 1
}

# Convert matrix to data frame
state_matrix_df <- as.data.frame(state_matrix)
state_matrix_df$grupo <- rownames(state_matrix)
rownames(state_matrix_df) <- NULL

# Save the result to an Excel file
output_path <- "groups_with_state_dummies.xlsx"
openxlsx::write.xlsx(state_matrix_df, output_path, rowNames = FALSE)

cat(paste("File saved as", output_path), "\n")

