# -----------------------------------------------------------------------------
# 1. SETUP — Libraries and file paths
# -----------------------------------------------------------------------------

.libPaths("/shared-directory/sd-tools/apps/R/lib/")

suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(did)          # Callaway & Sant'Anna DiD estimator
    library(ggplot2)
    library(gridExtra)    # arrangeGrob / grid.arrange for panel figures
    library(metafor)
    library(readr)
})

# --- Analysis date stamp (determines which result files are loaded) ----------
DATE <- "20260129"

# --- Input files -------------------------------------------------------------
dataset_file   <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE,'_FE_MetaAnalysis/Results_',          DATE, '/Results_ATC_',         DATE, '.csv')
events_file    <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE,'_FE_MetaAnalysis/ProcessedEvents_',  DATE, '/processed_events.parquet')
outcomes_file  <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE,'_FE_MetaAnalysis/ProcessedOutcomes_', DATE, '/processed_outcomes.parquet')
doctor_list    <- '/media/volume/Projects/DSGELabProject1/doctors_20250424.csv'
covariate_file <- '/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv'
renamed_ATC_file <- '/media/volume/Projects/ATC_renamed_codes.csv'

# --- Output directory --------------------------------------------------------
outdir <- '/media/volume/Projects/DSGELabProject1/Plots/Supplements/'
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# --- Parallelism -------------------------------------------------------------
N_THREADS <- 10
setDTthreads(N_THREADS)

# -----------------------------------------------------------------------------
# 2. SIGNIFICANCE FILTERING on the pre-computed results table
# -----------------------------------------------------------------------------

dataset <- read_csv(dataset_file, show_col_types = FALSE)

# Retain only ATC codes with a minimum number of cases for reliable estimation
dataset <- dataset[dataset$N_CASES >= 300, ]

# Bonferroni correction for the overall pre-to-post absolute change in
# prescription rate (tests whether any change occurred at all)
dataset$PVAL_ADJ_FDR      <- p.adjust(dataset$PVAL_ABS_CHANGE, method = "bonferroni")
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ_FDR < 0.05

# Bonferroni correction for the pre-period parallel-trends test (PVAL_PRE)
# and the post-period effect test (PVAL_POST).
# A result is considered "robust" when:
#   (a) pre-period prescription rates are NOT significantly different from
#       controls (parallel trends assumption holds), AND
#   (b) post-period prescription rates ARE significantly different from controls
dataset$PVAL_PRE_ADJ_FDR   <- p.adjust(dataset$PVAL_PRE,  method = "bonferroni")
dataset$PVAL_POST_ADJ_FDR  <- p.adjust(dataset$PVAL_POST, method = "bonferroni")
dataset$SIGNIFICANT_ROBUST <- (dataset$PVAL_PRE_ADJ_FDR  >= 0.05) &
                               (dataset$PVAL_POST_ADJ_FDR  < 0.05)

# Combined significance label used for downstream colouring / filtering
dataset$SIG_TYPE <- case_when(
    dataset$SIGNIFICANT_CHANGE & dataset$SIGNIFICANT_ROBUST ~ "Significant",
    TRUE ~ "Not Significant"
)

# -----------------------------------------------------------------------------
# 3. STATIN NAME LOOKUP AND COLOUR PALETTES
# -----------------------------------------------------------------------------

# Complete C10AA → generic drug name lookup
all_statin_names <- c(
    "C10AA01" = "simvastatin",
    "C10AA02" = "lovastatin",
    "C10AA03" = "pravastatin",
    "C10AA04" = "fluvastatin",
    "C10AA05" = "atorvastatin",
    "C10AA06" = "cerivastatin",
    "C10AA07" = "rosuvastatin"
)

# 7-colour palette for Plot A — one colour per statin, colourblind-friendly
palette_all <- c(
    "simvastatin"  = "#E63946",
    "lovastatin"   = "#9B59B6",
    "pravastatin"  = "#AF7AC5",
    "fluvastatin"  = "#F4A261",
    "atorvastatin" = "#3498DB",
    "cerivastatin" = "#16A085",
    "rosuvastatin" = "#000000"
)

# 3-colour subset for Plot B — picks the same hues as above for the focal trio
focal_statins <- c(
    "C10AA01" = "simvastatin",
    "C10AA05" = "atorvastatin",
    "C10AA07" = "rosuvastatin"
)
palette_focal <- palette_all[focal_statins]   # names are drug names


# -----------------------------------------------------------------------------
# 4. PLOT A — Statin prescription landscape
# -----------------------------------------------------------------------------

focal_code   <- "C10AA07"
atc_prefix   <- substr(focal_code, 1, 5)   # "C10AA"

# Load events and identify all individual sub-codes belonging to the ATC group
events_raw        <- as.data.table(read_parquet(events_file))
events_raw[, CODE := as.character(CODE)]
events_group      <- events_raw[startsWith(CODE, atc_prefix)]
atc_group_codes   <- unique(events_group$CODE)

# Load supporting reference files (shared across loops below)
doctor_ids    <- fread(doctor_list, header = FALSE)$V1
covariates_dt <- fread(covariate_file)
renamed_ATC   <- fread(renamed_ATC_file)

# Clean covariates: derive SPECIALTY and BIRTH_YEAR, drop raw columns
covariates_dt[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]

# Loop over each sub-code, build the panel data set, and store the average
# yearly prescription count (AVG_N) for use in the landscape plot.
landscape_results <- list()

for (atc_group_code in atc_group_codes) {

    outcome_code     <- atc_group_code
    event_actual_code <- atc_group_code

    # --- Reload events for this iteration ------------------------------------
    events_iter <- as.data.table(read_parquet(events_file))
    events_iter[, CODE := as.character(CODE)]

    # Skip deprecated ATC codes that have been renamed in the reference file
    if (event_actual_code %in% renamed_ATC$ATC_OLD) {
        cat(sprintf("Skipping %s: deprecated code (renamed).\n", event_actual_code))
        next
    }
    # If the code has superseded older codes, unify them under the new code
    if (event_actual_code %in% renamed_ATC$ATC_NEW) {
        old_codes <- renamed_ATC[ATC_NEW == event_actual_code, ATC_OLD]
        events_iter[CODE %in% old_codes, CODE := event_actual_code]
        cat(sprintf("%s is a new code; merged old codes: {%s}.\n",
                    event_actual_code, paste(old_codes, collapse = ", ")))
    }

    events_iter <- events_iter[startsWith(CODE, event_actual_code)]

    # --- Load outcomes for this sub-code -------------------------------------
    # Handle the case where the outcome column may be stored under an old code
    if (outcome_code %in% renamed_ATC$ATC_NEW) {
        outcome_cols <- c("DOCTOR_ID", "YEAR",
                          paste0("N_", outcome_code), paste0("Y_", outcome_code),
                          paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes_iter <- as.data.table(read_parquet(outcomes_file, col_select = outcome_cols))

        # Stack outcomes stored under the old codes after renaming their columns
        for (old_code in unique(renamed_ATC[ATC_NEW == outcome_code, ATC_OLD])) {
            old_cols <- c("DOCTOR_ID", "YEAR",
                          paste0("N_", old_code), paste0("Y_", old_code),
                          paste0("first_year_", old_code), paste0("last_year_", old_code))
            outcomes_old <- as.data.table(read_parquet(outcomes_file, col_select = old_cols))
            setnames(outcomes_old,
                     old = c(paste0("N_", old_code), paste0("Y_", old_code),
                             paste0("first_year_", old_code), paste0("last_year_", old_code)),
                     new = c(paste0("N_", outcome_code), paste0("Y_", outcome_code),
                             paste0("first_year_", outcome_code), paste0("last_year_", outcome_code)))
            outcomes_iter <- rbind(outcomes_iter, outcomes_old)
        }
    } else {
        outcome_cols  <- c("DOCTOR_ID", "YEAR",
                           paste0("N_", outcome_code), paste0("Y_", outcome_code),
                           paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes_iter <- as.data.table(read_parquet(outcomes_file, col_select = outcome_cols))
    }

    # Restrict to doctors in the validated doctor list
    outcomes_filtered <- outcomes_iter[DOCTOR_ID %in% doctor_ids]

    # --- Merge events with outcomes and add event indicator ------------------
    events_merged <- events_iter[, .(PATIENT_ID, CODE, DATE)]
    setnames(events_merged, "PATIENT_ID", "DOCTOR_ID")
    # Keep only the earliest event per doctor (first prescription date)
    events_merged <- events_merged[order(DOCTOR_ID, DATE)][, .SD[1], by = DOCTOR_ID]

    df_merged <- events_merged[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
    df_merged[, DATE       := as.Date(DATE)]
    df_merged[, EVENT      := ifelse(!is.na(DATE), 1, 0)]
    df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
    df_merged[, DATE       := NULL]

    # Merge doctor-level covariates (specialty, birth year, sex)
    df_complete <- covariates_dt[df_merged, on = "DOCTOR_ID"]
    df_complete[, `:=`(
        AGE          = YEAR - BIRTH_YEAR,
        AGE_IN_2023  = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]

    # --- Apply temporal buffer to avoid market entry / exit bias -------------
    # Prescriptions in the first and last calendar year of availability are
    # excluded because they reflect partial years rather than true rates.
    original_min_year <- min(df_complete[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
    original_max_year <- max(df_complete[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
    BUFFER_YEARS      <- 1
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    cat(sprintf("%-10s | Original range: %d-%d | Buffered range: %d-%d\n",
                outcome_code, original_min_year, original_max_year,
                buffered_min_year, buffered_max_year))

    df_complete <- df_complete[YEAR >= buffered_min_year & YEAR <= buffered_max_year]
    # Also exclude doctors whose prescribing event falls outside the buffered window
    df_complete <- df_complete[
        is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)
    ]

    # --- Pension-age filter --------------------------------------------------
    # Remove doctors who received (or prescribed) the statin after typical
    # retirement age, as their prescription behaviour is less interpretable.
    PENSION_AGE          <- 60
    events_after_pension <- df_complete[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT),unique(DOCTOR_ID)]
    df_complete <- df_complete[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]

    # --- Prepare the model data frame ----------------------------------------
    df_model <- as.data.table(df_complete)[, `:=`(
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
        SEX       = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
        Y         = get(paste0("Y_", outcome_code)),
        Ni        = get(paste0("N_", outcome_code))
    )]
    df_model[is.na(Y), Y := 0]   # Replace missing prescription ratios with 0

    # Assign did-package variables: unit ID, treatment cohort, and time
    df_model[, `:=`(
        ID = as.integer(factor(DOCTOR_ID)),
        G  = ifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR),
        T  = YEAR
    )]

    # Store the average prescription count per year for the landscape plot.
    # STATIN_NAME maps the ATC code to its generic drug name for axis labels.
    landscape_results[[atc_group_code]] <- df_model %>%
        group_by(YEAR) %>%
        summarise(
            CODE        = atc_group_code,
            STATIN_NAME = all_statin_names[atc_group_code],
            AVG_N       = mean(Ni, na.rm = TRUE),
            .groups     = "drop"
        ) %>%
        distinct()
}

# Combine results across all sub-codes
landscape_all <- do.call(rbind, landscape_results)

# Build Plot A — colour by statin name, not raw ATC code
plot_A <- ggplot(landscape_all, aes(x = YEAR, y = AVG_N,
                                    color = STATIN_NAME, group = STATIN_NAME)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = palette_all) +
    labs(
        x        = "Year",
        y        = "Average number of prescriptions",
        color    = "Statin name"
    ) +
    theme_minimal()

# Save Plot A individually
ggsave(
    file.path(outdir, "Supplementary_StationPrescriptionLandscape_20260310.png"),
    plot_A, width = 10, height = 6, dpi = 300
)

# -----------------------------------------------------------------------------
# 4. PLOT B — Zoom-in DiD results for the three most common statins
# -----------------------------------------------------------------------------

# Event code for the "treatment" event: a doctor's own first statin purchase.
# "Purch_C10AA" captures all statin purchases across the C10AA class.
event_code <- 'Purch_C10AA'

# Parse the event source (e.g. "Purch") and the ATC prefix from the combined string
event_code_parts  <- strsplit(event_code, "_")[[1]]
event_source      <- event_code_parts[1]
event_actual_code <- event_code_parts[2]

# Reload raw events and filter to the statin class
events_raw2 <- as.data.table(read_parquet(events_file))
events_statin <- events_raw2[
    SOURCE == event_source & startsWith(as.character(CODE), event_actual_code)
]

# Three focal statins (most commonly prescribed within C10AA).
# The lookup and palette subset are already defined in Section 3 above.

did_results_list <- list()

for (outcome_code in names(focal_statins)) {

    # --- Load outcomes for this statin ---------------------------------------
    outcomes_cols <- c("DOCTOR_ID", "YEAR",
                       paste0("N_", outcome_code), paste0("Y_", outcome_code),
                       paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
    outcomes_iter <- as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))

    # --- Filter events: for this outcome, the "event" is a doctor's first
    #     prescription of exactly this statin code
    events_focal <- events_statin[, .(PATIENT_ID, CODE, DATE)]
    events_focal <- events_focal[CODE == outcome_code]
    setnames(events_focal, "PATIENT_ID", "DOCTOR_ID")
    # Keep the earliest prescription date per doctor
    events_focal <- events_focal[order(DOCTOR_ID, DATE)][, .SD[1], by = DOCTOR_ID]

    # Restrict outcomes to the validated doctor list
    outcomes_filtered <- outcomes_iter[DOCTOR_ID %in% doctor_ids]

    # --- Merge events with outcomes ------------------------------------------
    df_merged <- events_focal[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
    df_merged[, DATE       := as.Date(DATE)]
    df_merged[, EVENT      := ifelse(!is.na(DATE), 1, 0)]
    df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
    df_merged[, DATE       := NULL]

    # Merge covariates
    df_complete <- covariates_dt[df_merged, on = "DOCTOR_ID"]
    df_complete[, `:=`(
        AGE          = YEAR - BIRTH_YEAR,
        AGE_IN_2023  = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]

    # --- Temporal buffer (same logic as Plot A) ------------------------------
    original_min_year <- min(df_complete[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
    original_max_year <- max(df_complete[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
    BUFFER_YEARS      <- 1
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    cat(sprintf("%-14s | Original range: %d-%d | Buffered range: %d-%d\n",
                focal_statins[outcome_code], original_min_year, original_max_year,
                buffered_min_year, buffered_max_year))

    df_complete <- df_complete[YEAR >= buffered_min_year & YEAR <= buffered_max_year]
    df_complete <- df_complete[
        is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)
    ]

    # --- Pension-age filter --------------------------------------------------
    PENSION_AGE          <- 60
    events_after_pension <- df_complete[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT),
                                        unique(DOCTOR_ID)]
    df_complete <- df_complete[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]

    # --- Prepare model data frame --------------------------------------------
    df_model <- as.data.table(df_complete)[, `:=`(
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
        SEX       = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
        Y         = get(paste0("Y_", outcome_code)),
        Ni        = get(paste0("N_", outcome_code))
    )]
    df_model[is.na(Y), Y := 0]

    # Restrict to events that occurred after 2008 (simvastatin drop starts) + 3 years (washout period before event) = 2011
    df_model <- df_model[is.na(EVENT_YEAR) | EVENT_YEAR > 2011]

    # Assign did-package variables
    df_model[, `:=`(
        ID = as.integer(factor(DOCTOR_ID)),
        G  = ifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR),
        T  = YEAR
    )]

    # --- Summary counts ------------------------------------------------------
    n_cases    <- uniqueN(df_model[EVENT == 1, DOCTOR_ID])
    n_controls <- uniqueN(df_model[EVENT == 0, DOCTOR_ID])
    events_per_year <- df_model[EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
    cat(sprintf("%-14s | Cases: %d | Controls: %d | Events/year: [%s]\n",
                focal_statins[outcome_code], n_cases, n_controls,
                paste0(events_per_year$EVENT_YEAR, ":", events_per_year$N, collapse = ", ")))

    # --- Callaway & Sant'Anna (2021) DiD estimation --------------------------
    # Doubly-robust estimator with "not-yet-treated" control group.
    # Standard errors are clustered at the doctor level.
    set.seed(09152024)
    att_gt_res <- att_gt(
        yname         = "Y",
        tname         = "T",
        idname        = "ID",
        gname         = "G",
        xformla       = ~ BIRTH_YEAR + SEX + SPECIALTY,
        data          = df_model,
        est_method    = "dr",
        control_group = "notyettreated",
        clustervars   = "ID",
        pl            = TRUE,
        cores         = N_THREADS
    )

    # Aggregate to a dynamic (event-study) ATT: average effect at each relative time
    agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)

    did_results_list[[outcome_code]] <- data.frame(
        time       = agg_dynamic$egt,
        att        = agg_dynamic$att.egt,
        se         = agg_dynamic$se.egt,
        outcome    = "Y",
        n_cases    = n_cases,
        n_controls = n_controls
    )
}

# Combine DiD results across the three statins
results_combined <- rbindlist(did_results_list, idcol = "outcome_code")

# Restrict to the ±3-year window around the event for plotting
data_plot <- results_combined[time >= -3 & time <= 3]
data_plot[, outcome_label := focal_statins[outcome_code]]

# Build Plot B — same colour hues as Plot A for the three focal statins
plot_B <- ggplot(data_plot, aes(x = time, y = att,color = outcome_label, group = outcome_label)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2,       position = position_dodge(width = 0.3)) +
    geom_errorbar(
        aes(ymin = att - se, ymax = att + se),
        width = 0.2, position = position_dodge(width = 0.3)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    scale_color_manual(values = palette_focal) +
    labs(
        x     = "Years from Event",
        y     = "Prescription Rate Difference\n(compared to controls)",
        color = "Statin name"
    ) +
    theme_minimal()

# Save Plot B individually
ggsave(
    file.path(outdir, "Supplementary_DiD_MostCommonStatins_20260310.png"),
    plot_B, width = 10, height = 6, dpi = 300
)


# -----------------------------------------------------------------------------
# 5. COMBINED PANEL FIGURE  (Plot A  |  Plot B)
#
#    Both plots are arranged side-by-side and labelled with panel letters
#    using gridExtra::arrangeGrob so the figure is self-contained for
#    inclusion in the supplementary materials.
# -----------------------------------------------------------------------------

# Remove legends from individual plots
plot_A_no_legend <- plot_A + theme(legend.position = "none")
plot_B_no_legend <- plot_B + theme(legend.position = "none")
# Transform into grobs with panel titles
grob_A_no_legend <- arrangeGrob(plot_A_no_legend,
                                top = grid::textGrob("A. Statin prescription landscape",
                                                     x = 0, hjust = 0,
                                                     gp = grid::gpar(fontface = "bold", fontsize = 13)))

grob_B_no_legend <- arrangeGrob(plot_B_no_legend,
                                top = grid::textGrob("B. Zoom-in results after 2011, for most common statins",
                                                     x = 0, hjust = 0,
                                                     gp = grid::gpar(fontface = "bold", fontsize = 13)))

# Extract shared legend in horizontal orientation
legend <- cowplot::get_legend(plot_A + theme(legend.direction = "horizontal"))
# Combine side-by-side with shared legend at bottom
combined_figure <- arrangeGrob(grob_A_no_legend, grob_B_no_legend, nrow = 1, bottom = legend)

# Save combined figure
ggsave(
    file.path(outdir, "Supplementary_RosuvastatinValidation_HOR_20260310.png"),
    combined_figure, width = 14, height = 6, dpi = 300
)
