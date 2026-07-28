# ============================================================
# Extra diagnoses: 
# DiD results for diagnoses that showed a significant effect on prescription volume 
# but were not part of the main analysis:
#   C50 = Malignant neoplasm of breast      (female doctors only)
#   I80 = Phlebitis and thrombophlebitis    (general, no sub-phenotype breakdown)
#   O02 = Other abnormal products of conception
#
# Pipeline (per diagnosis code):
#   1. Extract events -> first occurrence per doctor
#   2. Merge with prescription outcomes and doctor covariates
#   3. Run DiD model
#   4. Collect results
# 
# After the loop: 
# combine all codes into a single CSV and a single faceted comparison figure 
# ============================================================


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
    library(metafor)
    library(ggplot2)
})


# ============================================================
# 2. Paths - ALL input/output paths declared here
# ============================================================

# --- Date stamp used to build input file paths ---
DATE_DATA <- "20260427"
TODAY     <- format(Sys.time(), "%Y%m%d")   

# --- Input ---
PATH_DOCTOR_LIST     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
PATH_EVENTS_FILE     <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_", DATE_DATA, "/ProcessedEvents_", DATE_DATA, "/processed_events.parquet")
PATH_OUTCOMES_FILE   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_", DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
PATH_COVARIATES_FILE <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# --- Output ---
DIR_OUT <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)

FILE_RESULTS_CSV   <- paste0("Supplements_ExtraDiagnosis_All_", TODAY, ".csv")
FILE_PLOT_BASENAME <- paste0("Plot_Supplements_ExtraDiagnosis_All_", TODAY)

# ============================================================
# 3. Plotting parameters - ALL plot settings declared here
# ============================================================

WIN <- 3   # event-study window (years) shown in the plot

# -- Export settings --
PLOT_DPI    <- 300
PLOT_WIDTH  <- 8
PLOT_HEIGHT <- 12

# -- Colors / theme --
COLOR_LINE      <- "#1f77b4"
COLOR_ZERO_LINE <- "red"
THEME_BASE      <- theme_minimal()

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
# 4. Global settings
# ============================================================

N_THREADS <- 10
setDTthreads(N_THREADS)

# ICD-10 codes to analyze
EVENT_CODES <- list(
    CODE  = c("C50", "I80", "O02"),
    LABEL = c("Malignant neoplasm of breast", "Phlebitis and thrombophlebitis", "Other abnormal products of conception")
)

# ============================================================
# 5. Load shared data
# ============================================================

doctor_ids <- fread(PATH_DOCTOR_LIST, header = FALSE)$V1

# Covariates: keep specialty and birth year
covariates <- fread(PATH_COVARIATES_FILE)
covariates[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]
covariates[SPECIALTY == "", SPECIALTY := "No specialty"]

# Outcomes: total number of prescriptions per doctor per year
outcomes <- as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = c("DOCTOR_ID", "YEAR", "N")))

# ============================================================
# 6. Loop through each diagnosis code and perform DiD analysis
# ============================================================

results_list <- list()

for (code in unique(EVENT_CODES$CODE)) {

    pattern <- paste0("^", code)
    label   <- EVENT_CODES$LABEL[EVENT_CODES$CODE == code]

    cat(sprintf("Processing diagnosis: %s (%s)", label, code))

    # ------------------------------------------------------------
    # 6a. Extract events and define cohort
    # ------------------------------------------------------------

    # Load events and keep only the specified code
    events_raw <- as.data.table(read_parquet(PATH_EVENTS_FILE))
    events_raw <- events_raw[grepl(pattern, as.character(CODE), perl = TRUE), .(PATIENT_ID, CODE, DATE)]
    events_raw[, DATE := as.Date(DATE)]

    # Keep the first occurrence per patient x code
    events_raw <- events_raw[events_raw[, .I[which.min(DATE)], by = .(PATIENT_ID, CODE)]$V1]

    # Restrict to cohort doctors and take their earliest event across any matching code
    events_doctors <- events_raw[PATIENT_ID %in% doctor_ids, .(EVENT_DATE = min(DATE)), by = PATIENT_ID]
    setnames(events_doctors, "PATIENT_ID", "DOCTOR_ID")

    # ------------------------------------------------------------
    # 6b. Merge events, outcomes and covariates, then QC
    # ------------------------------------------------------------

    # Left join: all outcome rows kept; controls get NA event date
    df <- left_join(outcomes, events_doctors, by = "DOCTOR_ID") %>%
        mutate(
            EVENT      = if_else(!is.na(EVENT_DATE), 1L, 0L),
            EVENT_YEAR = if_else(!is.na(EVENT_DATE), as.numeric(format(EVENT_DATE, "%Y")), NA_real_)
        ) %>%
        select(-EVENT_DATE) %>%
        as.data.table()

    # Merge covariates
    df <- covariates[df, on = "DOCTOR_ID"]
    df[, `:=`(
        AGE          = YEAR - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]

    # Remove doctors whose event occurred after pension age (60)
    ids_post60 <- df[AGE_AT_EVENT > 60 & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
    df <- df[!(DOCTOR_ID %in% ids_post60) & AGE <= 60]

    # Replace missing prescription counts with 0
    df[is.na(N), N := 0]

    # DiD variables: numeric ID, group (first treatment year), calendar year
    df[, ID := as.integer(factor(DOCTOR_ID))]
    df[, G  := fifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)]
    df[, T  := YEAR]

    # For C50 (breast cancer), restrict to female doctors only
    if (code == "C50") {
        covariate_formula <- ~ BIRTH_YEAR + SPECIALTY
        df <- df[SEX == 2]
    } else {
        covariate_formula <- ~ BIRTH_YEAR + SPECIALTY + SEX
    }

    # ------------------------------------------------------------
    # 6c. DiD model
    # ------------------------------------------------------------

    n_cases    <- df[EVENT == 1, uniqueN(DOCTOR_ID)]
    n_controls <- df[EVENT == 0, uniqueN(DOCTOR_ID)]
    cat(sprintf("Final cohort: %d doctors (%d cases, %d controls)\n", n_cases + n_controls, n_cases, n_controls))

    # Baseline prescription rate in controls (used to express effects as % change)
    baseline <- df[EVENT == 0, mean(N, na.rm = TRUE)]

    att <- att_gt(
        yname = "N",
        tname = "T",
        idname = "ID",
        gname = "G",
        xformla = covariate_formula,
        data = df,
        est_method = "dr",
        control_group = "notyettreated",
        clustervars = "ID",
        pl = TRUE,
        cores = N_THREADS
    )
    agg <- aggte(att, type = "dynamic", na.rm = TRUE)
    results <- data.frame(
        time       = agg$egt,
        att        = agg$att.egt,
        se         = agg$se.egt,
        n_cases    = n_cases,
        n_controls = n_controls,
        baseline   = baseline
    )

    # Baseline & relative change estimates (effect size as % of control baseline)
    results <- results %>%
        mutate(
            rel_att    = round(100 * att / baseline, 5),
            rel_att_se = round(100 * se / baseline, 5)
        )

    # Tag with diagnosis code/label and restrict to the plotting event window before collecting 
    results <- results %>%
        mutate(CODE = code, LABEL = label) %>%
        filter(time >= -WIN & time <= WIN)

    results_list[[code]] <- results
}

# ============================================================
# 7. Combine all diagnoses into a single results file
# ============================================================

results_all <- rbindlist(results_list)

out_results_file <- file.path(DIR_OUT, FILE_RESULTS_CSV)
write.csv(results_all, out_results_file, row.names = FALSE)

# ============================================================
# 8. Combined comparison figure 
# ============================================================

# Reload the results to plot, so this step also works if run in a separate session
data_plot <- fread(out_results_file) %>%
    mutate(
        panel_title    = paste0("Results for: ", LABEL, " (ICD10 code: ", CODE, ")"),
        panel_subtitle = paste0("Cases: ", n_cases, ", Controls: ", n_controls)
    )

p <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = COLOR_LINE) +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = COLOR_LINE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_ZERO_LINE) +
    facet_wrap(~panel_title, ncol = 1) +
    labs(
        x = "Years from Event",
        y = "Change in Total Number of Prescriptions \n(compared to controls)"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    THEME_BASE +
    theme(strip.text = element_text(face = "bold"))

save_plot_png_pdf(p, DIR_OUT, FILE_PLOT_BASENAME, PLOT_WIDTH, PLOT_HEIGHT)