.libPaths("/shared-directory/sd-tools/apps/R/lib/")

#### Libraries:
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

##### Arguments
DATE = "20260129"
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE, '/Results_', DATE, '/Results_ATC_', DATE, '.csv')
events_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedEvents_", DATE, "/processed_events.parquet")
outcomes_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedOutcomes_", DATE, "/processed_outcomes.parquet")
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
renamed_ATC_file = "/media/volume/Projects/ATC_renamed_codes.csv"
outdir = "/media/volume/Projects/DSGELabProject1/Plots/Supplements/"
if (!dir.exists(outdir)) {dir.create(outdir, recursive = TRUE)}

dataset <- read_csv(dataset_file, show_col_types = FALSE)
# Filter only codes with at least 300 cases available
dataset <- dataset[dataset$N_CASES >= 300, ]

# STEP 1:
# Apply FDR multiple testing correction
dataset$PVAL_ADJ_FDR <- p.adjust(dataset$PVAL_ABS_CHANGE, method = "bonferroni")
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ_FDR < 0.05

# STEP 2:
# Select only robust results, i.e those point / events with:
# A. an average prescription rate before event significantly non-different from controls
# B. an average prescription rate after event significantly different from controls
# Also apply FDR multiple testing correction here
dataset$PVAL_PRE_ADJ_FDR <- p.adjust(dataset$PVAL_PRE, method = "bonferroni")
dataset$PVAL_POST_ADJ_FDR <- p.adjust(dataset$PVAL_POST, method = "bonferroni")    
dataset$SIGNIFICANT_ROBUST <- (dataset$PVAL_PRE_ADJ_FDR >= 0.05) & (dataset$PVAL_POST_ADJ_FDR < 0.05)

# STEP 3:
# Create a combined significance variable with two levels
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_CHANGE & dataset$SIGNIFICANT_ROBUST ~ "Significant",
  TRUE ~ "Not Significant"
)


# EXTRA:
# If temp_results file exists, do plot directly using it (skip all above)
results <- read_csv("/media/volume/Projects/DSGELabProject1/Plots/Supplements/temp_results_20260203.csv", show_col_types = FALSE)
code_list <- unique(results$code)

comparison_results <- data.frame()

for (code in code_list) {
    temp_results <- results %>% filter(code == !!code)
    
    # Define three experiments
    experiments <- list(
        exp1 = list(name = "Post Period = (+1, +2, +3)", method = "FE", before = c(-3, -2, -1), after = c(1, 2, 3)),
        exp2 = list(name = "Post Period = (0, +1, +2)", method = "FE", before = c(-3, -2, -1), after = c(0, 1, 2))    
    )
    for (exp in experiments) {
        before_idx <- temp_results$time %in% exp$before
        after_idx <- temp_results$time %in% exp$after
        
        # Meta-analysis of pre-period
        pre_data <- data.frame(
            estimate = temp_results$att[before_idx],
            se = temp_results$se[before_idx]
        )
        pre_meta <- metafor::rma(yi = estimate, sei = se, data = pre_data, method = exp$method)
        avg_effect_before <- pre_meta$b[,1]
        se_pre <- pre_meta$se
        p_value_pre <- pre_meta$pval
        
        # Meta-analysis of post-period
        post_data <- data.frame(
            estimate = temp_results$att[after_idx],
            se = temp_results$se[after_idx]
        )
        post_meta <- metafor::rma(yi = estimate, sei = se, data = post_data, method = exp$method)
        avg_effect_after <- post_meta$b[,1]
        se_post <- post_meta$se
        p_value_post <- post_meta$pval
        
        # Absolute change
        absolute_change <- avg_effect_after - avg_effect_before
        absolute_change_se <- sqrt(se_post^2 + se_pre^2)
        score_abs <- absolute_change / absolute_change_se
        abs_change_pval <- 2 * (1 - pnorm(abs(score_abs)))
        
        # Store results
        comparison_results <- rbind(comparison_results, data.frame(
            code = code,
            experiment = exp$name,
            absolute_change = round(absolute_change, 5),
            absolute_change_se = round(absolute_change_se, 5),
            p_value_change = round(abs_change_pval, 5)
        ))
    }
}

# Create scatter plot comparing absolute change coefficient estimates
comparison_pivot <- comparison_results %>%
    filter(code != "") %>%
    pivot_wider(names_from = experiment, values_from = c(absolute_change, absolute_change_se, p_value_change)) %>%
    filter(!is.na(`absolute_change_Post Period = (+1, +2, +3)`) & 
           !is.na(`absolute_change_Post Period = (0, +1, +2)`))

# Calculate 95% confidence intervals (1.96 * SE)
comparison_pivot <- comparison_pivot %>%
    mutate(
        ci_x_lower = `absolute_change_Post Period = (+1, +2, +3)` - 1.96 * `absolute_change_se_Post Period = (+1, +2, +3)`,
        ci_x_upper = `absolute_change_Post Period = (+1, +2, +3)` + 1.96 * `absolute_change_se_Post Period = (+1, +2, +3)`,
        ci_y_lower = `absolute_change_Post Period = (0, +1, +2)` - 1.96 * `absolute_change_se_Post Period = (0, +1, +2)`,
        ci_y_upper = `absolute_change_Post Period = (0, +1, +2)` + 1.96 * `absolute_change_se_Post Period = (0, +1, +2)`
    )

# Plot: Absolute change comparison with error bars
p <- ggplot(comparison_pivot, aes(x = `absolute_change_Post Period = (+1, +2, +3)`, y = `absolute_change_Post Period = (0, +1, +2)`)) +
    geom_errorbarh(aes(xmin = ci_x_lower, xmax = ci_x_upper), height = 0.0005, alpha = 0.4) +
    geom_errorbar(aes(ymin = ci_y_lower, ymax = ci_y_upper), width = 0.0005, alpha = 0.4) +
    geom_point(size = 2, alpha = 0.6) +
    ggrepel::geom_text_repel(aes(label = code), size = 3, max.overlaps = Inf) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    labs(title = "Absolute Change Comparison",
         x = "After (+1, +2, +3)",
         y = "After (0, +1, +2)") +
    theme_minimal() +
    xlim(0, 0.01) +
    ylim(0, 0.01)

ggsave(paste0(outdir, "Supplementary_PostDefinition_20260203.png"), plot = p, width = 6, height = 6)