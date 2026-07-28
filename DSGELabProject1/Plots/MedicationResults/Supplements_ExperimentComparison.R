# ============================================================
# Compare medication-level DiD results across three experiment specifications:
# - Base                        (3-year window, no shrinkage)                  
# - 5-year window               (5-year window, no shrinkage)
# - Empirical Bayes shrinkage   (3-year window, with shrinkage)
# ============================================================


# ============================================================
# 1. Libraries
# ============================================================

.libPaths("/shared-directory/sd-tools/apps/R/lib/")
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(readr)
})

# ============================================================
# 2. Paths and settings 
# ============================================================

TODAY <- format(Sys.Date(), "%Y%m%d")

# --- Input result files, one per experiment specification ---
PATH_FILES <- list(
    "Base"                       = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_20260129_FE_MetaAnalysis/Results_20260129/Results_ATC_20260129.csv",
    "5 Year Window"              = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_20260219_5years_window/Results_20260219/Results_ATC_20260219.csv",
    "Empirical Bayes Shrinkage"  = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_20260316/Results_20260316/Results_ATC_20260316.csv"
)

REPORTED_EXPERIMENT <- "Empirical Bayes Shrinkage"
OUTPUT_FILE         <- paste0("/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/Supplements_ExperimentComparison_", TODAY, ".csv")

# --- Filtering / significance settings ---
MIN_N_CASES  <- 300      
PVAL_METHOD  <- "bonferroni"
ALPHA        <- 0.05


# ============================================================
# 3. Load & Process each experiment's results  
# ============================================================

load_experiment_results <- function(path, min_n_cases, pval_method) {
    d <- fread(path)[N_CASES >= min_n_cases]
    d[, PVAL_ADJ := p.adjust(PVAL_ABS_CHANGE, method = pval_method)]
    d
}

experiment_results <- lapply(PATH_FILES, load_experiment_results, min_n_cases = MIN_N_CASES, pval_method = PVAL_METHOD)

# Check significant medications
sig_meds <- experiment_results[[REPORTED_EXPERIMENT]][PVAL_ADJ < ALPHA, OUTCOME_CODE]
cat(sprintf("Significant medications in the reported experiment ('%s'): %d\n", REPORTED_EXPERIMENT, length(sig_meds)))

# ============================================================
# 4. Build the comparison table
# ============================================================

comparison_list <- list()

for (exp in names(PATH_FILES)) {
    d <- experiment_results[[exp]][OUTCOME_CODE %in% sig_meds]
    comparison_list[[exp]] <- d[, .(
        ATC_CODE      = OUTCOME_CODE,
        EXPERIMENT    = exp,
        ABS_CHANGE_CI = sprintf("%.4f (%.4f, %.4f)", ABS_CHANGE, ABS_CHANGE - 1.96 * ABS_CHANGE_SE, ABS_CHANGE + 1.96 * ABS_CHANGE_SE),
        PVAL_ADJ      = PVAL_ADJ,
        SIGNIFICANT   = PVAL_ADJ < ALPHA
    )]
}

comparison_table <- rbindlist(comparison_list)[order(ATC_CODE, factor(EXPERIMENT, levels = names(PATH_FILES)))]
fwrite(comparison_table, OUTPUT_FILE)
