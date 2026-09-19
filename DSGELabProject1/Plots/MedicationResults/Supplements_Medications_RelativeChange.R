
# ============================================================
# 1. Libraries
# ============================================================
.libPaths("/shared-directory/sd-tools/apps/R/lib/")
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(did)
    library(ggplot2)
    library(patchwork)
    library(metafor)
    library(readr)
})


# ============================================================
# 2. Paths
# ============================================================

DATE_DATA <- "20260918"   
TODAY     <- format(Sys.Date(), "%Y%m%d")

# -- Input --
PATH_MAIN_RESULTS    <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/Results_", DATE_DATA, "/Results_ATC_", DATE_DATA, ".csv")
PATH_EVENTS_FILE     <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedEvents_", DATE_DATA, "/processed_events.parquet")
PATH_OUTCOMES_FILE   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
PATH_DOCTOR_LIST     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"

# -- Output --
DIR_OUT <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)


BASENAME_RELCHANGE_PLOT        <- paste0("Supplements_RelativeChange_Plot_", TODAY)
FILE_RELCHANGE_ESTIMATES_CSV    <- paste0("Supplements_RelativeChange_Estimates_", TODAY, ".csv")


# ============================================================
# 3. Parameters 
# ============================================================

# -- Cohort / significance settings --
MIN_N_CASES <- 300        
PVAL_METHOD <- "bonferroni"
ALPHA       <- 0.05

BUFFER_YEARS <- 1  

# -- Export settings  --
PLOT_DPI <- 300
PLOT_WIDTH_BASELINE_EVOLUTION  <- 10
PLOT_HEIGHT_BASELINE_EVOLUTION <- 6
PLOT_WIDTH_RELCHANGE  <- 12
PLOT_HEIGHT_RELCHANGE <- 8

# -- Colors / theme --
COLOR_REF_LINE   <- "grey"
COLOR_HIGHLIGHT  <- "red"   
THEME_BASE       <- theme_minimal()

# -- Helper: save a ggplot as both PNG and PDF using the same base filename --
save_plot_png_pdf <- function(plot, dir, basename, width, height, dpi = PLOT_DPI) {
    ggsave(filename = file.path(dir, paste0(basename, ".png")), 
      plot = plot,
      width = width, 
      height = height, 
      dpi = dpi
    )
    ggsave(filename = file.path(dir, paste0(basename, ".pdf")), 
      plot = plot,
      width = width, 
      height = height
    )
}

# ============================================================
# 4. Load the results table and process it
# ============================================================

dataset <- read_csv(PATH_MAIN_RESULTS, show_col_types = FALSE) %>%
    select(
        outcome_code,
        absolute_change,
        absolute_change_se,
        p_value_change,
        baseline,
        relative_change,
        n_cases
    ) %>%
    rename(
        OUTCOME_CODE    = outcome_code,
        ABS_CHANGE      = absolute_change,
        ABS_CHANGE_SE   = absolute_change_se,
        PVAL_ABS_CHANGE = p_value_change,
        BASELINE_MEAN   = baseline,
        REL_CHANGE      = relative_change,
        N_CASES         = n_cases
    )
dataset <- dataset[dataset$N_CASES >= MIN_N_CASES, ]

# Apply multiple test correction (on the overall abs-change p-value)
dataset$PVAL_ADJ <- p.adjust(dataset$PVAL_ABS_CHANGE, method = PVAL_METHOD)
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ < ALPHA
dataset$SIG_TYPE <- case_when(
    dataset$SIGNIFICANT_CHANGE ~ "Significant",
    TRUE ~ "Not Significant"
)

# Extract list of significant medications, plus the manually added extras
code_list <- dataset %>%
    filter(SIG_TYPE == "Significant") %>%
    pull(OUTCOME_CODE) %>%
    unique()

# Process data
dataset <- dataset %>%
    filter(OUTCOME_CODE %in% code_list) %>%
    mutate(
        REL_CHANGE_SE     = abs(ABS_CHANGE_SE / BASELINE_MEAN),
        REL_CHANGE_CI_LOW = REL_CHANGE - 1.96 * REL_CHANGE_SE,
        REL_CHANGE_CI_UP  = REL_CHANGE + 1.96 * REL_CHANGE_SE,
        PVAL_REL_CHANGE   = 2 * (1 - pnorm(abs((REL_CHANGE - 1) / REL_CHANGE_SE)))
    )

# Plot: Relative change with CI and p-value annotations
p <- ggplot(dataset, aes(y = reorder(OUTCOME_CODE, REL_CHANGE))) +
    geom_vline(xintercept = 1, linetype = "dashed", color = COLOR_REF_LINE, linewidth = 0.8) +
    geom_point(aes(x = REL_CHANGE), color = COLOR_HIGHLIGHT, size = 3, fill = COLOR_HIGHLIGHT) +
    geom_errorbarh(aes(xmin = REL_CHANGE_CI_LOW, xmax = REL_CHANGE_CI_UP), height = 0.2, color = COLOR_HIGHLIGHT, linewidth = 0.8) +
    geom_text(aes(x = REL_CHANGE, label = paste0(round(REL_CHANGE, 3), "\n[", round(REL_CHANGE_CI_LOW, 3), ", ", round(REL_CHANGE_CI_UP, 3), "]")), hjust = -0.1, vjust = 0.5, size = 4) +
    labs(
        title = "Estimated Relative Change in Prescription Rates After Event (95% CI)",
        x     = "Relative Change",
        y     = "ATC Code"
    ) +
    THEME_BASE +
    theme(axis.text.y = element_text(size = 8), legend.position = "bottom")

save_plot_png_pdf(p, DIR_OUT, BASENAME_RELCHANGE_PLOT, PLOT_WIDTH_RELCHANGE, PLOT_HEIGHT_RELCHANGE)


# ============================================================
# 5. Save final combined results (single CSV)
# ============================================================

dataset_with_baseline <- dataset_with_baseline %>%
    mutate(
        ABS_CHANGE_CI_LOW = ABS_CHANGE - 1.96 * ABS_CHANGE_SE,
        ABS_CHANGE_CI_UP  = ABS_CHANGE + 1.96 * ABS_CHANGE_SE
    ) %>%
    select(
        OUTCOME_CODE, BASELINE_MEAN,
        ABS_CHANGE, REL_CHANGE,
        ABS_CHANGE_SE, REL_CHANGE_SE,
        ABS_CHANGE_CI_LOW, ABS_CHANGE_CI_UP,
        REL_CHANGE_CI_LOW, REL_CHANGE_CI_UP,
        PVAL_ABS_CHANGE, PVAL_REL_CHANGE
    )

write_csv(dataset_with_baseline, file.path(DIR_OUT, FILE_RELCHANGE_ESTIMATES_CSV))