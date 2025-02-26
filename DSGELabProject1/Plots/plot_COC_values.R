library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)

# load data
df = fread("/media/volume/Projects/DSGELabProject1/patient_COC_info_20250226.csv")
doc_ids = fread("/media/volume/Projects/DSGELabProject1/doctors_20250220.csv", header=FALSE)$V1

# summarize COC results
summary(df$COC)

# histogram of COC values
p1 <- ggplot(df, aes(x=COC)) + 
    geom_histogram(binwidth=0.05, color='black', alpha=0.5) + 
    geom_vline(xintercept = c(0.4, 0.7), color = "red", linetype = "dashed") +
    labs(x = "Bice-Boxerman Continuity of Care (COC) Index", y = "Count") +
    theme_minimal()

# Further analysis of COC values based on:
# A. Number of doctors vs COC values
p2 <- ggplot(df, aes(x=COC, y=TOTAL_VISITS)) + 
    geom_point(alpha=0.5) +
    labs(x = "Bice-Boxerman Continuity of Care (COC) ",y = "Index Total N. of Visits") +
    theme_minimal()


# B. COC values for doctors vs non-doctors
df <- df %>% mutate(is_doctor = as.factor(ifelse(PATIENT_ID %in% doc_ids, "Doctor", "Not Doctor")))
p3 <- ggplot(df, aes(x=COC, fill=is_doctor)) + 
    geom_density(alpha=0.5) + 
    geom_vline(xintercept = c(0.4, 0.7), color = "red", linetype = "dashed") +
    labs(x = "Bice-Boxerman Continuity of Care (COC) Index", y = "Density", fill = 'Patient type') +
    theme_minimal()

# Arrange plots
combined_plot <- p1 | (p2 / p3)

# Save plot
outpath <- "/media/volume/Projects/DSGELabProject1/Plots/plot_COC_values_distribution_20250226.png"
ggsave(filename = outpath, plot = combined_plot, width = 14, height = 8, dpi = 300)