.libPaths("/shared-directory/sd-tools/apps/R/lib/")

#### Libraries:
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(readr)
})

files <- list(
  "Original"                                = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_20260129_FE_MetaAnalysis/Results_20260129/Results_ATC_20260129.csv",
  "5 Year Window"                           = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_20260219_5years_window/Results_20260219/Results_ATC_20260219.csv",
  "Empirical Bayes Shrinkage (Reported)"    = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_20260316/Results_20260316/Results_ATC_20260316.csv"
)

output_file_1 <- "/media/volume/Projects/DSGELabProject1/Plots/ExperimentComparisonTable_ALL_20260316.csv"
output_file_2 <- "/media/volume/Projects/DSGELabProject1/Plots/ExperimentComparisonTable_ONLY_20260316.csv"

 
# ── Pass 1: collect significant medication codes ──────────────────────────────
 
sig_meds <- c()

for (exp in names(files)) {
    d <- fread(files[[exp]])[N_CASES >= 300]
    d[, PVAL_ADJ := p.adjust(PVAL_ABS_CHANGE, method = "bonferroni")]
    sig_meds <- union(sig_meds, d[PVAL_ADJ < 0.05, OUTCOME_CODE])
}

cat(sprintf("Significant medications across all experiments: %d\n", length(sig_meds)))

# ── Pass 2: extract full results for significant medications ──────────────────

results <- list()

for (exp in names(files)) {
    d <- fread(files[[exp]])[N_CASES >= 300]
    d[, PVAL_ADJ := p.adjust(PVAL_ABS_CHANGE, method = "bonferroni")]
    d <- d[OUTCOME_CODE %in% sig_meds]
    results[[exp]] <- d[, .(
        MED_CODE      = OUTCOME_CODE,
        EXPERIMENT    = exp,
        ABS_CHANGE_CI = sprintf("%.4f (%.4f, %.4f)", ABS_CHANGE, ABS_CHANGE - 1.96*ABS_CHANGE_SE, ABS_CHANGE + 1.96*ABS_CHANGE_SE),
        PVAL_ADJ      = PVAL_ADJ,
        SIGNIFICANT   = PVAL_ADJ < 0.05
    )]
}
out <- rbindlist(results)[order(MED_CODE, factor(EXPERIMENT, levels = names(files)))]
 
fwrite(out, output_file_1)
cat(sprintf("Saved: %s\n", output_file_1))

# ── Pass 3: extract results for medications significant in all experiments ───────────────────

out_all <- out[, all(SIGNIFICANT), by = MED_CODE][V1 == TRUE, MED_CODE]
out_filtered <- out[MED_CODE %in% out_all]

fwrite(out_filtered, output_file_2)
cat(sprintf("Saved: %s\n", output_file_2))
