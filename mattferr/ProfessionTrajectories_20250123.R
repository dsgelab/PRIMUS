library(TraMineR)
library(data.table)
library(lubridate)

# SET PATHS HERE:
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
Valvira_header_path = "~/Desktop/shared-directory/data_dictionaries/Valvira/FD_2698_Liite1"
plot_outpath = "~/Desktop/shared-directory/mattferr/state_matrix_plot.pdf"

# load data
# df = fread(Valvira_path, nrows = 1000 ,encoding = "Latin-1") # TESTING
# df = fread(Valvira_path, encoding = "Latin-1")
header = fread(Valvira_header_path)$new_name
colnames(df) = header
df$PROFESSION_START_DATE = as.Date(df$PROFESSION_START_DATE, format = "%d.%m.%Y")
df$PROFESSION_END_DATE = as.Date(df$PROFESSION_END_DATE, format = "%d.%m.%Y")

# Expand data into a profession-by-year format
expand_data <- function(df) {
  expanded <- do.call(rbind, lapply(1:nrow(df), function(i) {
    if (is.na(df$PROFESSION_START_DATE[i]) || is.na(df$PROFESSION_END_DATE[i])) {
      return(NULL) 
    }
    start_year <- as.numeric(format(df$PROFESSION_START_DATE[i], "%Y"))
    end_year <- as.numeric(format(df$PROFESSION_END_DATE[i], "%Y"))  
    if (is.na(start_year) || is.na(end_year) || start_year > end_year) {
      return(NULL) 
    }
    years <- seq(from = start_year, to = end_year)
    data.frame(ID = df$ID[i], YEAR = years, PROFESSION = df$PROFESSION[i])
  })) 
  return(expanded)
}
expanded_df = expand_data(df)

# Create a WIDE format profession-by-year matrix for sequence analysis
# stopping in 2025 (data is only available until 2023) for better visualization
unique_years <- sort(unique(expanded_df$YEAR))
unique_ids <- unique(expanded_df$ID)
state_matrix <- matrix("None", 
                       nrow = length(unique_ids), 
                       ncol = length(unique_years),
                       dimnames = list(as.character(unique_ids), as.character(unique_years)))

for (i in seq_len(nrow(expanded_df))) {
  id <- as.character(expanded_df$ID[i])
  year <- as.character(expanded_df$YEAR[i])
  profession <- expanded_df$PROFESSION[i]
  if (year %in% colnames(state_matrix)) {state_matrix[id, year] <- profession}
}

# Plotting
unique_professions <- unique(as.vector(state_matrix))
ordered_professions <- sort(unique_professions)
state_matrix_numeric <- state_matrix_numeric[ordered_ids, ]
legend_labels <- ordered_professions
legend_colors <- heat.colors(length(legend_labels))
years <- colnames(state_matrix)


image(
  1:ncol(state_matrix_numeric), 
  1:nrow(state_matrix_numeric), 
  t(state_matrix_numeric), 
  main = "State Matrix", 
  xlab = "Year", ylab = "ID", 
  axes = FALSE, 
  col = legend_colors)
axis(1, at = 1:ncol(state_matrix_numeric), labels = years)
legend(
  "topright", 
  legend = legend_labels, 
  fill = legend_colors, 
  title = "Profession", 
  cex = 0.7, ncol = 1, bty = "n")


#### Sequence clustering analysis ####
seq_obj = seqdef(state_matrix)
dist_matrix = seqdist(seq_obj, method = "OM", sm="TRATE") 
clusters = hclust(as.dist(dist_matrix), method = "ward.D2") 

# Compute elbow plot from WSS to determine optimal number of clusters
K_RANGE = 2:10
wss = sapply(K_RANGE, function(k) {
  cluster_groups = cutree(clusters, k = k)
  sum(sapply(unique(cluster_groups), function(group) {
    members = which(cluster_groups == group)
    sum(dist_matrix[members, members]^2) / length(members)
  }))
})
plot(K_RANGE, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Method for Optimal Clusters")

# Assign clusters
K = 3
cluster_groups = cutree(clusters, k = K) 
cluster_assignments = data.frame(ID = unique_ids, Cluster = cluster_groups)
expanded_df = merge(expanded_df, cluster_assignments, by = "ID", all.x = TRUE)

# Visualize sequences
seqdplot(
  seq_obj, 
  group = cluster_groups, 
  main = "State Distribution Plot by Cluster", 
  with.legend = "right")

# Extract representative sequences 
representative_seqs <- seqrep(seq_obj, diss = dist_matrix, nrep = K)
print(representative_seqs)