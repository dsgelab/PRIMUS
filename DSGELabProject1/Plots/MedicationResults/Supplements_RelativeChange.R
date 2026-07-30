
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

DATE_DATA <- "20260316"   
TODAY     <- format(Sys.Date(), "%Y%m%d")

# -- Input --
PATH_MAIN_RESULTS    <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/Results_", DATE_DATA, "/Results_ATC_", DATE_DATA, ".csv")
PATH_EVENTS_FILE     <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedEvents_", DATE_DATA, "/processed_events.parquet")
PATH_OUTCOMES_FILE   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
PATH_DOCTOR_LIST     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"

# -- Output --
DIR_OUT <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)

BASENAME_EVOLUTION_PLOT         <- paste0("Supplements_RelativeChange_BaselinePrescription_", TODAY)
BASENAME_RELCHANGE_PLOT1        <- paste0("Supplements_RelativeChange_V1_", TODAY)
BASENAME_RELCHANGE_PLOT2        <- paste0("Supplements_RelativeChange_V2_", TODAY)
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
# 4. Load the main results table and identify significant medications
# ============================================================

dataset <- read_csv(PATH_MAIN_RESULTS, show_col_types = FALSE)
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

# ============================================================
# 5. Part 1: baseline prescription rates per medication
# ============================================================

doctor_ids <- fread(PATH_DOCTOR_LIST, header = FALSE)$V1
events <- as.data.table(read_parquet(PATH_EVENTS_FILE))
events[, CODE := as.character(CODE)]

baseline_overall_list <- list()
baseline_by_year_list <- list()

for (code in code_list) {
    # Filter events based on the event code
    events_new  <- events[CODE == code & SOURCE == "Purch"]
    event_ids   <- unique(events_new$PATIENT_ID)
    control_ids <- setdiff(doctor_ids, event_ids)

    # Load outcomes
    outcome_cols <- c("DOCTOR_ID", "YEAR", paste0("Y_", code), paste0("first_year_", code), paste0("last_year_", code))
    outcomes <- as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = outcome_cols))

    # Trim the medication's on-market window (avoid bias from the drug
    # entering/exiting the market during the study period)
    original_min_year <- min(outcomes[[paste0("first_year_", code)]], na.rm = TRUE)
    original_max_year <- max(outcomes[[paste0("last_year_", code)]], na.rm = TRUE)
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    outcomes <- outcomes[YEAR >= buffered_min_year & YEAR <= buffered_max_year]

    outcomes_controls <- outcomes %>% filter(DOCTOR_ID %in% control_ids)

    # Overall baseline mean (used for the relative-change calculation in Part 2)
    baseline_overall <- outcomes_controls %>%
        summarise(BASELINE_MEAN = mean(get(paste0("Y_", code)), na.rm = TRUE), .groups = "drop")
    baseline_overall_list[[code]] <- data.frame(
        OUTCOME_CODE  = code,
        BASELINE_MEAN = baseline_overall$BASELINE_MEAN
    )

    # Baseline mean by year (used only for the QC evolution plot below)
    baseline_by_year <- outcomes_controls %>%
        group_by(YEAR) %>%
        summarise(BASELINE_MEAN = mean(get(paste0("Y_", code)), na.rm = TRUE), .groups = "drop")
    baseline_by_year_list[[code]] <- data.frame(
        OUTCOME_CODE  = code,
        YEAR          = baseline_by_year$YEAR,
        BASELINE_MEAN = baseline_by_year$BASELINE_MEAN
    )
}

baseline_rates_df    <- do.call(rbind, baseline_overall_list)
baseline_rates_by_year <- do.call(rbind, baseline_by_year_list)


# ============================================================
# 6. QC plot: evolution of baseline prescription rates over the years
# ============================================================

p_baseline_evolution <- ggplot(baseline_rates_by_year, aes(x = YEAR, y = BASELINE_MEAN, color = OUTCOME_CODE, group = OUTCOME_CODE)) +
    geom_line() +
    geom_point() +
    labs(
        title = "Baseline Prescription Rates Evolution Over Years",
        x     = "Year",
        y     = "Baseline Rate",
        color = "ATC Code"
    ) +
    THEME_BASE +
    theme(legend.position = "right")

save_plot_png_pdf(p_baseline_evolution, DIR_OUT, BASENAME_EVOLUTION_PLOT, PLOT_WIDTH_BASELINE_EVOLUTION, PLOT_HEIGHT_BASELINE_EVOLUTION)

# ============================================================
# 7. Part 2: relative change in prescription rate after the event,
# ============================================================

dataset_with_baseline <- dataset %>%
    filter(OUTCOME_CODE %in% code_list) %>%
    left_join(baseline_rates_df, by = "OUTCOME_CODE")

# Calculate relative change: (post-event level) / baseline
dataset_with_baseline <- dataset_with_baseline %>%
    mutate(REL_CHANGE = (ABS_CHANGE + BASELINE_MEAN) / BASELINE_MEAN) %>%
    arrange(REL_CHANGE)

# ------------------------------------------------------------
# 7a. baseline vs. after-event prescription rate (absolute scale)
# ------------------------------------------------------------
p1 <- ggplot(dataset_with_baseline, aes(y = reorder(OUTCOME_CODE, REL_CHANGE))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = COLOR_REF_LINE, linewidth = 0.8) +
    geom_segment(aes(x = BASELINE_MEAN, xend = BASELINE_MEAN + ABS_CHANGE, yend = reorder(OUTCOME_CODE, REL_CHANGE)), color = COLOR_HIGHLIGHT, linewidth = 0.8) +
    geom_point(aes(x = BASELINE_MEAN, shape = "Baseline"), color = COLOR_HIGHLIGHT, size = 3, stroke = 1.5) +
    geom_point(aes(x = BASELINE_MEAN + ABS_CHANGE, shape = "After Event"), color = COLOR_HIGHLIGHT, size = 4) +
    scale_shape_manual(values = c("Baseline" = 1, "After Event" = 16), name = "") +
    labs(
        title = "Estimated Change in Average Prescription Rates After Event (Relative to Baseline)",
        x     = "Prescription Rate",
        y     = "ATC Code"
    ) +
    THEME_BASE +
    theme(axis.text.y = element_text(size = 8), legend.position = "bottom")

save_plot_png_pdf(p1, DIR_OUT, BASENAME_RELCHANGE_PLOT1, PLOT_WIDTH_RELCHANGE, PLOT_HEIGHT_RELCHANGE)


# ------------------------------------------------------------
# 7b. relative change with 95% CI, and a test of H0: relative change = 1
# ------------------------------------------------------------
dataset_with_baseline <- dataset_with_baseline %>%
    mutate(
        REL_CHANGE_SE     = abs(ABS_CHANGE_SE / BASELINE_MEAN),
        REL_CHANGE_CI_LOW = REL_CHANGE - 1.96 * REL_CHANGE_SE,
        REL_CHANGE_CI_UP  = REL_CHANGE + 1.96 * REL_CHANGE_SE,
        PVAL_REL_CHANGE   = 2 * (1 - pnorm(abs((REL_CHANGE - 1) / REL_CHANGE_SE)))
    )

p2 <- ggplot(dataset_with_baseline, aes(y = reorder(OUTCOME_CODE, REL_CHANGE))) +
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

save_plot_png_pdf(p2, DIR_OUT, BASENAME_RELCHANGE_PLOT2, PLOT_WIDTH_RELCHANGE, PLOT_HEIGHT_RELCHANGE)


# ============================================================
# 8. Save final combined results (single CSV)
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