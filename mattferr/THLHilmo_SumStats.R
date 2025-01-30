library(data.table)
library(dplyr)
library(ggplot2)

# SET PATHS HERE:
THL_path <- "/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/"
Hilmo_header_path <- "~/Desktop/shared-directory/data_dictionaries/THL/Avohilmo_11_19"
header = fread(Hilmo_header_path)$new_name

data_outpath <- "~/Desktop/shared-directory/data_dictionaries/THL_Hilmo_SumStats_20250129.csv"
plot1_outpath = "~/Desktop/shared-directory/data_dictionaries/THL_Hilmo_SumStats_unique_values_20250129/"
plot2_outpath = "~/Desktop/shared-directory/data_dictionaries/THL_Hilmo_SumStats_pct_missing_20250129/"

#### Functions ####
get_sumstats = function(data){
    N = nrow(data)
    result = list()
    for(col in colnames(data)){
        unique_values = length(unique(data[[col]]))
        pct_missing = sum(is.na(data[[col]]))*100/N
        result[[col]] = list(unique_values = unique_values, pct_missing = pct_missing)
    }
    return(result)
}
# fetch data
file_paths = list.files(path = THL_path, full.names = TRUE, pattern = "HILMO\\d+")
file_paths = file_paths[order(grepl("9806", file_paths), decreasing = TRUE)]
year_list = seq(1, 5, by = 1)

# extract summary stats for each year
summary_df = data.frame(year = integer(), variable = character(), unique_values = integer(), pct_missing = numeric(), stringsAsFactors = FALSE)

for(i in 1:length(file_paths)){
  print(file_paths[i])
  #data = fread(file_paths[i], nrows=1000, encoding = "Latin-1") # TESTING  
  #data = fread(file_paths[i], encoding = "Latin-1")
  colnames(data) = header
  sumstats = get_sumstats(data)
  sumstats_df = data.frame(year = year_list[i], variable = names(sumstats), unique_values = sapply(sumstats, function(x) x$unique_values), pct_missing = sapply(sumstats, function(x) x$pct_missing))
  summary_df = bind_rows(summary_df, sumstats_df)
}

# save dataframe to csv file
write.csv(summary_df, data_outpath, row.names = FALSE)
# reload data
# summary_df = fread(data_outpath)

# Reshape the dataframe for plotting
summary_long = data.frame(year = integer(), variable = character(), stat = character(), value = numeric(), stringsAsFactors = FALSE)
for(i in 1:nrow(summary_df)){
  summary_long = rbind(summary_long, data.frame(year = summary_df$year[i], variable = summary_df$variable[i], stat = "unique_values", value = summary_df$unique_values[i]))
  summary_long = rbind(summary_long, data.frame(year = summary_df$year[i], variable = summary_df$variable[i], stat = "pct_missing", value = summary_df$pct_missing[i]))
}

# Create directories if they don't exist
dir.create(plot1_outpath, showWarnings = FALSE, recursive = TRUE)
dir.create(plot2_outpath, showWarnings = FALSE, recursive = TRUE)

# plot unique values and missing percentages for each variable
variables <- unique(summary_long$variable)
for (v in variables) {
  print(v)
  unique_values_plot <- ggplot(subset(summary_long, stat == "unique_values" & variable == v), aes(x = year, y = value, color = variable)) +
  geom_line(linewidth=0.7) +
  labs(y = "N of Unique Values", x = "Year", title = paste("Unique Values for", v)) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10), legend.position = "bottom")
  ggsave(filename = paste0(plot1_outpath, v, "_unique_values.pdf"), plot = unique_values_plot, width = 11, height = 5, dpi = 150, units = "in", device = "pdf")
  
  pct_missing_plot <- ggplot(subset(summary_long, stat == "pct_missing" & variable == v), aes(x = year, y = value, color = variable)) +
  geom_line(linewidth=0.7) +
  labs(y = "Missing values (%)", x = "Year", title = paste("Percentage of Missing for", v)) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10), legend.position = "bottom")
  ggsave(filename = paste0(plot2_outpath, v, "_pct_missing.pdf"), plot = pct_missing_plot, width = 11, height = 5, dpi = 150, units = "in", device = "pdf")
}