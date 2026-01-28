# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(patchwork)
library(ggrepel)

# Global Variables
DATE = "20260123"
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE, '/Results_', DATE, '/Results_ATC_', DATE, '.csv')
OutDir <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Supplements/")
if (!dir.exists(OutDir)) {dir.create(OutDir, recursive = TRUE)}

# ============================================================================
# Scatter plot of FDR significant points only
# ============================================================================
# Main 
dataset <- read_csv(dataset_file, show_col_types = FALSE)
# Filter only codes with at least 300 cases available
dataset <- dataset[dataset$N_CASES >= 300, ]

# Calculate p-values and FDR correction
dataset <- dataset %>% mutate(
    Z_SCORE = ABS_CHANGE / ABS_CHANGE_SE,
    PVAL = 2 * (1 - pnorm(abs(Z_SCORE)))
)
dataset$PVAL_ADJ_FDR <- p.adjust(dataset$PVAL, method = "fdr")
dataset$SIGNIFICANT_FDR <- dataset$PVAL_ADJ_FDR < 0.05
dataset$SIGNIFICANT_ROBUST <- (dataset$PVAL_PRE >= 0.05) & (dataset$PVAL_POST < 0.05)
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_FDR & dataset$SIGNIFICANT_ROBUST ~ "Significant",
  TRUE ~ "Not Significant"
)

# Extract MED_CHAPTER (first character of OUTCOME_CODE)
dataset <- dataset %>% mutate(MED_CHAPTER = substr(OUTCOME_CODE, 1, 1))

# Plot p-value (pre-event vs post-event) scatter for FDR significant points only
p <- ggplot(dataset %>% filter(SIGNIFICANT_FDR), aes(x = PVAL_PRE, y = PVAL_POST, color = SIG_TYPE)) +
    geom_point(alpha = 0.6, size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
    geom_text_repel(aes(label = OUTCOME_CODE), size = 3, 
                    max.overlaps = Inf, 
                    box.padding = 0.5, 
                    point.padding = 0.5,
                    segment.size = 0.3,
                    segment.alpha = 0.6) +
    labs(title = "Pre vs Post event average effect comparison (against controls), for FDR significant change in prescription events",
         subtitle = "Quadrant 2 (P-value pre-event > 0.05, P-value post-event < 0.05) is the one of interest",
         x = "P-value pre-event", 
         y = "P-value post-event") +
    scale_x_continuous(breaks = c(0, 0.05, 0.5, 1), labels = c("0", "0.05", "0.5", "1"), limits = c(0, 1)) +
    scale_y_continuous(breaks = c(0, 0.05, 0.5, 1), labels = c("0", "0.05", "0.5", "1"), limits = c(0, 1)) +
    scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue")) +
    coord_fixed() +
    theme_minimal()

# Combine the two plots using patchwork
ggsave(paste0(OutDir, "Supplementary_Pvalues_", DATE, ".png"), p, width = 8, height = 6)
