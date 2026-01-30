# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(patchwork)
library(ggrepel)

# Global Variables
DATE = "20260129"
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

# STEP 1:
# Apply FDR multiple testing correction
dataset$PVAL_ADJ_FDR <- p.adjust(dataset$PVAL_ABS_CHANGE, method = "fdr")
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ_FDR < 0.05

# STEP 2:
# Select only robust results, i.e those point / events with:
# A. an average prescription rate before event significantly non-different from controls
# B. an average prescription rate after event significantly different from controls
# Also apply FDR multiple testing correction here
dataset$PVAL_PRE_ADJ_FDR <- p.adjust(dataset$PVAL_PRE, method = "fdr")
dataset$PVAL_POST_ADJ_FDR <- p.adjust(dataset$PVAL_POST, method = "fdr")    
dataset$SIGNIFICANT_ROBUST <- (dataset$PVAL_PRE_ADJ_FDR >= 0.05) & (dataset$PVAL_POST_ADJ_FDR < 0.05)

# STEP 3:
# Create a combined significance variable with two levels
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_CHANGE & dataset$SIGNIFICANT_ROBUST ~ "Robustly Significant",
  TRUE ~ "Not Robustly Significant"
)

# Extract MED_CHAPTER (first character of OUTCOME_CODE)
dataset <- dataset %>% mutate(MED_CHAPTER = substr(OUTCOME_CODE, 1, 1))

# Plot p-value (pre-event vs post-event) scatter for FDR significant points only
p <- ggplot(dataset %>% filter(SIGNIFICANT_CHANGE), aes(x = PVAL_PRE_ADJ_FDR, y = PVAL_POST_ADJ_FDR, color = SIG_TYPE)) +
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
    labs(title = "Pre-event vs Post-event estimated effect comparison (against controls), for FDR significant change in prescription events",
         subtitle = "Robustly Significant : (adj. p-value pre-event > 0.05, adj. p-value post-event < 0.05) ",
         x = "P-value pre-event", 
         y = "P-value post-event") +
    scale_x_continuous(breaks = c(0, 0.05, 0.5, 1), labels = c("0", "0.05", "0.5", "1"), limits = c(0, 1)) +
    scale_y_continuous(breaks = c(0, 0.05, 0.5, 1), labels = c("0", "0.05", "0.5", "1"), limits = c(0, 1)) +
    scale_color_manual(values = c("Robustly Significant" = "red", "Not Robustly Significant" = "blue")) +
    coord_fixed() +
    theme_minimal()

# Combine the two plots using patchwork
ggsave(paste0(OutDir, "Supplementary_Pvalues_", DATE, ".png"), p, width = 16, height = 14)
