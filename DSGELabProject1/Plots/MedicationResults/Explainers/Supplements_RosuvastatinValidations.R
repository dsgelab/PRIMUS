### ----------------------------------------------------------------------------
### SUPPLEMENTARY ANALYSIS — Rosuvastatin validation
###
### DESIGN
### The event is ALWAYS the same: the doctor's first prescription of the focal
### medication (rosuvastatin). Cases, controls, event timing and the analytic
### sample are therefore constructed ONCE and never change.
### What changes across models is the OUTCOME: one DiD model per medication
### (focal drug + declared standards of care), all sharing the same event.
### This removes the competing-event problem of the previous set-up, where every
### medication had its own event definition and therefore its own case/control
### sets, making estimates non-comparable across medications.
###
### OUTPUT
###   Panel A — prescription landscape of the whole medication class
###   Panel B — DiD event-study estimates, one line per outcome medication
###   CSV     — landscape data, plotting data, full estimates, exposure overlap
### ----------------------------------------------------------------------------


### ----------------------------------------------------------------------------
### 0. LIBRARIES
### ----------------------------------------------------------------------------

.libPaths("/shared-directory/sd-tools/apps/R/lib/")

suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(did)
    library(ggplot2)
    library(gridExtra)
    library(readr)
})


### ----------------------------------------------------------------------------
### 1. PARAMETERS / ARGUMENTS
### ----------------------------------------------------------------------------

# --- Analysis date stamp (determines which result files are loaded/written) ---
DATE <- "20260316"

# --- Event definition (IDENTICAL for every model in this script) --------------
EVENT_SOURCE <- "Purch"       # registry source of the event
FOCAL_CODE   <- "C10AA07"     # rosuvastatin: the drug whose first use is the event
ATC_PREFIX   <- substr(FOCAL_CODE, 1, 5)   # "C10AA" — the medication class

# --- Outcomes: declared standards of care tested against the same event ------
# The focal drug is kept as the first outcome: it anchors the plot and acts as
# the positive control (prescribing of the drug that defines the event).
COMPARATOR_CODES <- c("C10AA01", "C10AA05")   # simvastatin, atorvastatin
OUTCOME_CODES    <- unique(c(FOCAL_CODE, COMPARATOR_CODES))

# --- Sample construction -----------------------------------------------------
BUFFER_YEARS   <- 1      # drop first/last year of focal-drug availability
PENSION_AGE    <- 60     # doctors older than this are excluded
EVENT_YEAR_MIN <- 2011   # keep events STRICTLY after this year (2008 + 3y washout)
MIN_N_GENERAL  <- 5      # min. yearly prescriptions per doctor (Panel A only)
N_THRESHOLD    <- 5      # empirical-Bayes shrinkage threshold (Panel B models)

# --- DiD model ---------------------------------------------------------------
MODEL_COVARIATES <- ~ BIRTH_YEAR + SEX + SPECIALTY
EST_METHOD       <- "dr"              # doubly robust
CONTROL_GROUP    <- "notyettreated"
SEED             <- 09152024
N_THREADS        <- 10
setDTthreads(N_THREADS)

# --- Exposure-overlap diagnostic (which drugs the cases actually tried) ------
PRE_WINDOW  <- -3:-1
POST_WINDOW <- 1:3

# --- Plotting ----------------------------------------------------------------
PLOT_WINDOW   <- c(-3, 3)   # event-time window shown in Panel B
CI_MULTIPLIER <- 1.96       # set to 1 to reproduce the +/- 1 SE bars of the old figure
DODGE_WIDTH   <- 0.3
FIG_WIDTH_SINGLE   <- 10
FIG_HEIGHT_SINGLE  <- 6
FIG_WIDTH_COMBINED <- 14
FIG_HEIGHT_COMBINED <- 6
FIG_DPI <- 300

# --- Reference: medication names and colour palette (whole ATC class) --------
medication_names <- c(
    "C10AA01" = "simvastatin",
    "C10AA02" = "lovastatin",
    "C10AA03" = "pravastatin",
    "C10AA04" = "fluvastatin",
    "C10AA05" = "atorvastatin",
    "C10AA06" = "cerivastatin",
    "C10AA07" = "rosuvastatin"
)

palette_medications <- c(
    "simvastatin"  = "#E63946",
    "lovastatin"   = "#9B59B6",
    "pravastatin"  = "#AF7AC5",
    "fluvastatin"  = "#F4A261",
    "atorvastatin" = "#3498DB",
    "cerivastatin" = "#16A085",
    "rosuvastatin" = "#000000"
)

# Colours used in Panel B (subset of the class palette, so both panels match)
palette_outcomes <- palette_medications[medication_names[OUTCOME_CODES]]


### ----------------------------------------------------------------------------
### 2. PATHS
### ----------------------------------------------------------------------------

# --- Inputs ------------------------------------------------------------------
events_file      <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE, '/ProcessedEvents_',   DATE, '/processed_events.parquet')
outcomes_file    <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE, '/ProcessedOutcomes_', DATE, '/processed_outcomes.parquet')
doctor_list      <- '/media/volume/Projects/DSGELabProject1/doctors_20250424.csv'
covariate_file   <- '/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv'

# --- Output directory --------------------------------------------------------
outdir <- '/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/Explainers/'
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# --- Output files ------------------------------------------------------------
file_plot_A        <- paste0(outdir, "Supplements_PrescriptionLandscape_",   DATE, ".png")
file_plot_B        <- paste0(outdir, "Supplements_DiD_MultipleOutcomes_",    DATE, ".png")
file_plot_combined <- paste0(outdir, "Supplements_RosuvastatinValidation_",  DATE, ".png")
file_csv_landscape <- paste0(outdir, "Supplements_PanelA_LandscapeData_",    DATE, ".csv")
file_csv_plotdata  <- paste0(outdir, "Supplements_PanelB_PlotData_",         DATE, ".csv")
file_csv_estimates <- paste0(outdir, "Supplements_DiD_Estimates_Full_",      DATE, ".csv")
file_csv_overlap   <- paste0(outdir, "Supplements_CaseExposureOverlap_",     DATE, ".csv")


### ----------------------------------------------------------------------------
### 3. SHARED INPUT DATA
### Loaded once and reused by both panels.
### ----------------------------------------------------------------------------

# --- Doctor list -------------------------------------------------------------
doctor_ids <- fread(doctor_list, header = FALSE)$V1

# --- Doctor characteristics: derive SPECIALTY and BIRTH_YEAR, drop raw cols ---
covariates_dt <- fread(covariate_file)
covariates_dt[, `:=`(
    SPECIALTY      = as.character(INTERPRETATION),
    BIRTH_YEAR     = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE     = NULL,
    INTERPRETATION = NULL
)]

# --- Events ------------------------------------------------------------------
events_raw <- as.data.table(read_parquet(events_file))
events_raw[, CODE := as.character(CODE)]

# --- Outcome file schema (used to check which codes are actually available) ---
outcomes_schema <- open_dataset(outcomes_file)$schema$names

# Stop early if a declared outcome has no columns in the outcome file
missing_outcomes <- OUTCOME_CODES[!paste0("Y_", OUTCOME_CODES) %in% outcomes_schema]
if (length(missing_outcomes) > 0) {
    stop("Outcome columns missing from the outcome file for: ", paste(missing_outcomes, collapse = ", "))
}


### ----------------------------------------------------------------------------
### 4. PANEL A — Prescription landscape of the medication class
### Purely descriptive: average number of prescriptions per doctor-year for
### every drug of the class. No event logic is involved here.
### ----------------------------------------------------------------------------

# Codes of the class that are both named above and present in the outcome file
landscape_codes <- names(medication_names)[
    startsWith(names(medication_names), ATC_PREFIX) &
    paste0("N_", names(medication_names)) %in% outcomes_schema
]

landscape_results <- list()

for (code_i in landscape_codes) {

    # --- Load the columns of this drug only ----------------------------------
    cols_i <- c("DOCTOR_ID", "YEAR", "N_general",
                paste0(c("N_", "first_year_", "last_year_"), code_i))
    outcomes_i <- as.data.table(read_parquet(outcomes_file, col_select = cols_i))

    # Restrict to the validated doctor list
    outcomes_i <- outcomes_i[DOCTOR_ID %in% doctor_ids]

    # --- Add doctor characteristics and age ----------------------------------
    landscape_i <- covariates_dt[outcomes_i, on = "DOCTOR_ID"]
    landscape_i[, AGE := YEAR - BIRTH_YEAR]

    # --- Temporal buffer: drop the first and last year of availability -------
    # Those years are partial and therefore under-count prescriptions.
    original_min_year <- suppressWarnings(min(landscape_i[[paste0("first_year_", code_i)]], na.rm = TRUE))
    original_max_year <- suppressWarnings(max(landscape_i[[paste0("last_year_",  code_i)]], na.rm = TRUE))
    if (!is.finite(original_min_year) || !is.finite(original_max_year)) {
        cat(sprintf("%-10s | no prescriptions in the data — skipped\n", code_i))
        next
    }
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    cat(sprintf("%-10s (%-12s) | availability: %d-%d | buffered: %d-%d\n",
                code_i, medication_names[code_i],
                original_min_year, original_max_year,
                buffered_min_year, buffered_max_year))

    landscape_i <- landscape_i[YEAR >= buffered_min_year & YEAR <= buffered_max_year]

    # --- Pension-age filter --------------------------------------------------
    landscape_i <- landscape_i[AGE <= PENSION_AGE]

    # --- Keep doctors with enough prescriptions in every year they appear ----
    docs_to_keep <- landscape_i[, .(all_years_ok = all(N_general >= MIN_N_GENERAL)), by = DOCTOR_ID][all_years_ok == TRUE, DOCTOR_ID]
    landscape_i  <- landscape_i[DOCTOR_ID %in% docs_to_keep]

    # --- Average yearly prescription count of this drug ----------------------
    landscape_i[, Ni := get(paste0("N_", code_i))]
    landscape_results[[code_i]] <- landscape_i[, .(
        CODE            = code_i,
        MEDICATION_NAME = medication_names[code_i],
        AVG_N           = mean(Ni, na.rm = TRUE),
        N_DOCTORS       = uniqueN(DOCTOR_ID)
    ), by = YEAR]
}

landscape_all <- rbindlist(landscape_results)

# --- Build Panel A -----------------------------------------------------------
plot_A <- ggplot(landscape_all, aes(x = YEAR, y = AVG_N, color = MEDICATION_NAME, group = MEDICATION_NAME)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = palette_medications) +
    labs(
        x     = "Year",
        y     = "Average number of prescriptions",
        color = "Medication name"
    ) +
    theme_minimal()

ggsave(file_plot_A, plot_A, width = FIG_WIDTH_SINGLE, height = FIG_HEIGHT_SINGLE, dpi = FIG_DPI)


### ----------------------------------------------------------------------------
### 5. ANALYTIC SAMPLE — built ONCE, shared by every outcome model
### Event = first prescription of the focal medication.
### Cases = doctors with such an event inside the study window.
### Controls = doctors without any focal-medication prescription.
### ----------------------------------------------------------------------------

# --- Event: earliest focal-drug prescription per doctor ----------------------
events_focal <- events_raw[SOURCE == EVENT_SOURCE & CODE == FOCAL_CODE, .(PATIENT_ID, DATE)]
setnames(events_focal, c("PATIENT_ID", "DATE"), c("DOCTOR_ID", "EVENT_DATE"))
events_focal[, EVENT_DATE := as.Date(EVENT_DATE)]
events_focal <- events_focal[order(DOCTOR_ID, EVENT_DATE)][, .SD[1], by = DOCTOR_ID]

# --- Outcomes: load every outcome column in one pass -------------------------
outcome_cols <- unique(c(
    "DOCTOR_ID", "YEAR", "N_general",
    paste0("N_",          OUTCOME_CODES),
    paste0("Y_",          OUTCOME_CODES),
    paste0("first_year_", OUTCOME_CODES),
    paste0("last_year_",  OUTCOME_CODES)
))
outcomes_all <- as.data.table(read_parquet(outcomes_file, col_select = outcome_cols))

# Restrict to the validated doctor list
outcomes_all <- outcomes_all[DOCTOR_ID %in% doctor_ids]

# --- Merge event onto the doctor-year panel and flag treatment ---------------
df_analysis <- events_focal[outcomes_all, on = "DOCTOR_ID"]
df_analysis[, EVENT      := fifelse(!is.na(EVENT_DATE), 1L, 0L)]
df_analysis[, EVENT_YEAR := as.numeric(format(EVENT_DATE, "%Y"))]
df_analysis[, EVENT_DATE := NULL]

# --- Merge doctor characteristics -------------------------------------------
df_analysis <- covariates_dt[df_analysis, on = "DOCTOR_ID"]
df_analysis[, `:=`(
    AGE          = YEAR - BIRTH_YEAR,
    AGE_IN_2023  = 2023 - BIRTH_YEAR,
    AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
)]

# --- Study window: driven by the FOCAL drug only -----------------------------
# The window is defined by the event medication (not by each outcome), so that
# the sample stays identical across all outcome models.
original_min_year <- min(df_analysis[[paste0("first_year_", FOCAL_CODE)]], na.rm = TRUE)
original_max_year <- max(df_analysis[[paste0("last_year_",  FOCAL_CODE)]], na.rm = TRUE)
study_min_year    <- original_min_year + BUFFER_YEARS
study_max_year    <- original_max_year - BUFFER_YEARS
cat(sprintf("\nStudy window from focal drug %s (%s): %d-%d (buffered from %d-%d)\n",
            FOCAL_CODE, medication_names[FOCAL_CODE],
            study_min_year, study_max_year, original_min_year, original_max_year))

df_analysis <- df_analysis[YEAR >= study_min_year & YEAR <= study_max_year]
# Doctors whose event falls outside the window are removed entirely (they are
# neither clean cases nor clean controls)
df_analysis <- df_analysis[is.na(EVENT_YEAR) | (EVENT_YEAR >= study_min_year & EVENT_YEAR <= study_max_year)]

# --- Pension-age filter ------------------------------------------------------
events_after_pension <- df_analysis[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
df_analysis <- df_analysis[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]


# --- Model variables ---------------------------------------------------------
df_analysis[, `:=`(
    SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(SPECIALTY), ""))),
    SEX       = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    ID        = as.integer(factor(DOCTOR_ID)),
    G         = fifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR),
    T         = YEAR
)]

# --- Sample size (identical for every outcome — this is the point of the design)
n_cases    <- uniqueN(df_analysis[EVENT == 1, DOCTOR_ID])
n_controls <- uniqueN(df_analysis[EVENT == 0, DOCTOR_ID])
cat(sprintf("Shared analytic sample: %d cases, %d controls\n\n", n_cases, n_controls))


### ----------------------------------------------------------------------------
### 6. DIAGNOSTIC — which medications the cases actually prescribed
### Share of cases with at least one prescription of each outcome medication
### before and after their focal-drug event. Documents the exposure overlap
### that the previous (competing-event) design left implicit.
### ----------------------------------------------------------------------------

cases_dt <- df_analysis[EVENT == 1]
overlap_results <- list()

for (code_i in OUTCOME_CODES) {

    tmp <- cases_dt[, .(DOCTOR_ID,
                        REL_TIME = YEAR - EVENT_YEAR,
                        Ni       = get(paste0("N_", code_i)))]
    tmp[is.na(Ni), Ni := 0]

    pre_i  <- tmp[REL_TIME %in% PRE_WINDOW,  .(any_use = any(Ni > 0)), by = DOCTOR_ID]
    post_i <- tmp[REL_TIME %in% POST_WINDOW, .(any_use = any(Ni > 0)), by = DOCTOR_ID]

    overlap_results[[code_i]] <- data.table(
        outcome_code       = code_i,
        outcome_label      = medication_names[code_i],
        n_cases            = n_cases,
        n_cases_pre        = nrow(pre_i),
        pct_used_before    = 100 * mean(pre_i$any_use),
        n_cases_post       = nrow(post_i),
        pct_used_after     = 100 * mean(post_i$any_use)
    )
}

overlap_all <- rbindlist(overlap_results)
print(overlap_all)


### ----------------------------------------------------------------------------
### 7. DiD MODELS — same event, one model per outcome medication
### Callaway & Sant'Anna (2021), doubly-robust, not-yet-treated controls,
### standard errors clustered at the doctor level.
### ----------------------------------------------------------------------------

did_results_list <- list()

for (outcome_code in OUTCOME_CODES) {

    # --- Availability check: the outcome drug should cover the whole window --
    outcome_min_year <- min(df_analysis[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
    outcome_max_year <- max(df_analysis[[paste0("last_year_",  outcome_code)]], na.rm = TRUE)
    if (outcome_min_year > study_min_year || outcome_max_year < study_max_year) {
        cat(sprintf("WARNING | %s (%s) available %d-%d, study window is %d-%d — outcome partly reflects market availability\n",
                    outcome_code, medication_names[outcome_code],
                    outcome_min_year, outcome_max_year, study_min_year, study_max_year))
    }

    # --- Copy the shared sample and attach THIS outcome ----------------------
    df_model <- copy(df_analysis)
    df_model[, Y := get(paste0("Y_", outcome_code))]
    df_model[is.na(Y), Y := 0]

    # --- Empirical Bayes shrinkage for low-volume doctor-years ---------------
    # Years with few total prescriptions give unstable ratios; they are shrunk
    # towards the doctor's own mean, computed on the years above the threshold.
    df_model[, Y_MEAN := mean(Y[N_general >= N_THRESHOLD], na.rm = TRUE), by = DOCTOR_ID]
    df_model[, Y := fifelse(
        N_general < N_THRESHOLD & is.finite(Y_MEAN),
        (N_general * Y + N_THRESHOLD * Y_MEAN) / (N_general + N_THRESHOLD),
        Y
    )]
    df_model[, Y_MEAN := NULL]

    # --- Estimation ----------------------------------------------------------
    cat(sprintf("Fitting DiD | event: %s | outcome: %s (%s)\n",
                medication_names[FOCAL_CODE], outcome_code, medication_names[outcome_code]))
    set.seed(SEED)
    att_gt_res <- att_gt(
        yname         = "Y",
        tname         = "T",
        idname        = "ID",
        gname         = "G",
        xformla       = MODEL_COVARIATES,
        data          = df_model,
        est_method    = EST_METHOD,
        control_group = CONTROL_GROUP,
        clustervars   = "ID",
        pl            = TRUE,
        cores         = N_THREADS
    )

    # Aggregate to a dynamic (event-study) ATT: average effect per relative year
    agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)

    did_results_list[[outcome_code]] <- data.table(
        outcome_code  = outcome_code,
        outcome_label = medication_names[outcome_code],
        event_code    = FOCAL_CODE,
        event_label   = medication_names[FOCAL_CODE],
        time          = agg_dynamic$egt,
        att           = agg_dynamic$att.egt,
        se            = agg_dynamic$se.egt,
        n_cases       = n_cases,
        n_controls    = n_controls
    )
}

results_combined <- rbindlist(did_results_list)


### ----------------------------------------------------------------------------
### 8. RESULT TABLES — export everything used downstream
### ----------------------------------------------------------------------------

# Plotting subset: event-time window plus confidence bounds
data_plot <- results_combined[time >= PLOT_WINDOW[1] & time <= PLOT_WINDOW[2]]
data_plot[, `:=`(
    ci_low  = att - CI_MULTIPLIER * se,
    ci_high = att + CI_MULTIPLIER * se
)]
data_plot[, outcome_label := factor(outcome_label, levels = medication_names[OUTCOME_CODES])]

write_csv(landscape_all,    file_csv_landscape)   # Panel A data
write_csv(data_plot,        file_csv_plotdata)    # Panel B data (as plotted)
write_csv(results_combined, file_csv_estimates)   # full event-study estimates
write_csv(overlap_all,      file_csv_overlap)     # case exposure overlap


### ----------------------------------------------------------------------------
### 9. PANEL B — DiD estimates, one line per outcome medication
### ----------------------------------------------------------------------------

plot_B <- ggplot(data_plot, aes(x = time, y = att, color = outcome_label, group = outcome_label)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = DODGE_WIDTH)) +
    geom_point(size = 2,       position = position_dodge(width = DODGE_WIDTH)) +
    geom_errorbar(
        aes(ymin = ci_low, ymax = ci_high),
        width = 0.2, position = position_dodge(width = DODGE_WIDTH)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    scale_color_manual(values = palette_outcomes) +
    labs(
        x     = paste0("Years from first ", medication_names[FOCAL_CODE], " prescription"),
        y     = "Prescription Rate Difference\n(compared to controls)",
        color = "Medication name"
    ) +
    theme_minimal()

ggsave(file_plot_B, plot_B, width = FIG_WIDTH_SINGLE, height = FIG_HEIGHT_SINGLE, dpi = FIG_DPI)


### ----------------------------------------------------------------------------
### 10. COMBINED FIGURE — Panel A | Panel B, with a shared legend
### ----------------------------------------------------------------------------

# Remove legends from the individual plots
plot_A_no_legend <- plot_A + theme(legend.position = "none")
plot_B_no_legend <- plot_B + theme(legend.position = "none")

# Turn each plot into a grob carrying its panel title
grob_A_no_legend <- arrangeGrob(
    plot_A_no_legend,
    top = grid::textGrob(
        paste0("A. ", medication_names[FOCAL_CODE], " class prescription landscape"),
        x = 0, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 13)
    )
)

grob_B_no_legend <- arrangeGrob(
    plot_B_no_legend,
    top = grid::textGrob(
        paste0("B. Prescribing change after first ", medication_names[FOCAL_CODE],
               " prescription, by medication"),
        x = 0, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 13)
    )
)

# Shared horizontal legend taken from Panel A (covers the whole class)
legend <- cowplot::get_legend(plot_A + theme(legend.direction = "horizontal"))

combined_figure <- arrangeGrob(grob_A_no_legend, grob_B_no_legend, nrow = 1, bottom = legend)

ggsave(file_plot_combined, combined_figure,
       width = FIG_WIDTH_COMBINED, height = FIG_HEIGHT_COMBINED, dpi = FIG_DPI)