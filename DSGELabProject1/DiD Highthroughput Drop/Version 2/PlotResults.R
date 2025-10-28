# Prescription Event Analysis - Visualization Script
# Data: EVENT_CODE, BASELINE, DROP, TTR, N_CASES, N_CONTROLS

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(patchwork)
library(viridis)

# Load data
DATE = "20251014"
data <- read.csv(paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput_drop/Results/DropAnalysisResults_", DATE, ".csv"))
OutDir <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput_drop/Results/PlotResults_", DATE, "/")
if (!dir.exists(OutDir)) {dir.create(OutDir, recursive = TRUE)}

# ============================================================================
# DROP ANALYSIS
# Analyze the immediate impact (prescription drops) after events
# ============================================================================

# Relative drop (as ATT from DiD framework)
# Boxplot of event across different diagnosis chapters
data_diag <- data %>% filter(grepl("^Diag", EVENT_CODE))
data_diag$GROUP <- substr(sub(".*_", "", data_diag$EVENT_CODE), 1, 1)
data_diag$PVAL <- 2 * (1 - pnorm(abs(data_diag$ATT_DROP / data_diag$SE_DROP)))

# Apply Bonferroni correction
data_diag$PVAL_ADJ_BONFERRONI <- p.adjust(data_diag$PVAL, method = "bonferroni")
data_diag$PVAL_ADJ_FDR <- p.adjust(data_diag$PVAL, method = "fdr")
data_diag$SIGNIFICANT_FDR <- data_diag$PVAL_ADJ_FDR < 0.05
data_diag$SIG_TYPE <- ifelse(data_diag$SIGNIFICANT_FDR, "Significant", "Not Significant")

# Sort GROUP alphabetically for x axis
data_diag$GROUP <- factor(data_diag$GROUP, levels = sort(unique(data_diag$GROUP)))

# Create label (manually) for significant results
icd10_map <- c(
  # Chapter II: Neoplasms
  "C50" = "Malignant neoplasm of breast",
  
  # Chapter V: Mental and behavioural disorders
  "F33" = "Recurrent depressive disorder",
  "F43" = "Reaction to severe stress and adjustment disorders",
  
  # Chapter XV: Pregnancy, childbirth and the puerperium
  "O02" = "Other abnormal products of conception",
  "O13" = "Gestational hypertension",
  "O24" = "Diabetes mellitus in pregnancy",
  "O32" = "Maternal care for malpresentation of fetus",
  "O34" = "Maternal care for abnormality of pelvic organs",
  "O36" = "Maternal care for other fetal problems",
  "O42" = "Premature rupture of membranes",
  "O47" = "False labour",
  "O80" = "Single spontaneous delivery",
  "O81" = "Single delivery by forceps and vacuum extractor",
  "O82" = "Single delivery by caesarean section",
  "O99" = "Other maternal diseases classifiable elsewhere",
  
  # Chapter XXI: Factors influencing health status
  "Z34" = "Supervision of normal pregnancy",
  "Z35" = "Supervision of high-risk pregnancy",
  "Z36" = "Antenatal screening",
  "Z39" = "Postpartum care and examination",
  "Z25" = "Need for immunization against other viral diseases"
)

# Extract ICD-10 code from EVENT_CODE and map to description
data_diag$EVENT_DESCRIPTION <- sapply(data_diag$EVENT_CODE, function(x) {
  # Extract the 3-digit ICD code (e.g., "Diag_C50" -> "C50")
  icd_code <- sub(".*_([A-Z][0-9]{2}).*", "\\1", x)
  # Look up description, return code if not found
  ifelse(icd_code %in% names(icd10_map), icd10_map[icd_code], icd_code)
})

p_diag <- ggplot(data_diag, aes(x = GROUP, y = ATT_DROP, fill = GROUP)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(aes(color = SIG_TYPE, alpha = SIG_TYPE), width = 0.2, size = 2, show.legend = TRUE) +
  geom_text(data = filter(data_diag, SIGNIFICANT_FDR), aes(label = EVENT_DESCRIPTION), vjust = 2, size = 2.5) +
  scale_fill_manual(values = rep("gray70", nlevels(data_diag$GROUP))) +
  scale_color_manual(
    name = "Significance \n (FDR corrected P-value < 0.05)",
    values = c("Significant" = "black", "Not Significant" = "grey")
  ) +
  scale_alpha_manual(
    name = "Significance \n (FDR corrected P-value < 0.05)",
    values = c("Significant" = 1, "Not Significant" = 0.2)
  ) +
  labs(
    title = "Drop in Total Prescriptions due to Health Event, by Diagnosis Chapter",
    subtitle = sprintf("Number of diagnosis tested: %d", nrow(data_diag)),
    x = "ICD-10 Diagnosis Chapter",
    y = "Average Change in Total Prescriptions After Disease \n(Difference-in-Differences Estimate)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(),
    legend.text = element_text(),
    plot.margin = margin(10, 10, 10, 10, "pt")
  ) +
  guides(fill = "none") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_cartesian(clip = "off")

# Arrange plots side by side, and save
ggsave(filename = paste0(OutDir, "DropPlot_", DATE, ".png"), plot = p_diag, width = 12, height = 5, device = "png")
