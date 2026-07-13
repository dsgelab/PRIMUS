
# ============================================================
# This script extracts results and plots for diagnosis that shows significant effect on prescription volume:
# C50 = Malignant neoplasm of breast
# I80 = Phlebitis and thrombophlebitis
# O02 = Other abnormal products of conception
# but were not included in the main analysis
# =============================================================

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
# 2. File paths and global settings
# ============================================================

DATE_DATA <- "20260427"

doctor_list     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
events_file     <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_",DATE_DATA, "/ProcessedEvents_",   DATE_DATA, "/processed_events.parquet")
outcomes_file   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_",DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
covariates_file <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

TODAY <- format(Sys.time(), "%Y%m%d")
outdir   <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_ExtraDiagnosis_", TODAY, "/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

N_THREADS  <- 10
setDTthreads(N_THREADS)

# ICD-10 codes to analyze
EVENT_CODES <- list(
    CODE = c("C50", "I80", "O02"),
    LABEL = c("Malignant neoplasm of breast", "Phlebitis and thrombophlebitis", "Other abnormal products of conception")
)

# Window size for plot
WIN <- 3

# ============================================================
# 3. Load shared data
# ============================================================

doctor_ids <- fread(doctor_list, header = FALSE)$V1

# Covariates: keep specialty and birth year
covariates <- fread(covariates_file)
covariates[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]
covariates[SPECIALTY == "", SPECIALTY := "No specialty"]

# Outcomes: total number of prescriptions per doctor per year
outcomes <- as.data.table(read_parquet(outcomes_file, col_select = c("DOCTOR_ID", "YEAR", "N")))

# ============================================================
# 4. Loop through each diagnosis code and perform DiD analysis
# ============================================================

summary_list <- list()

for (code in unique(EVENT_CODES$CODE)) {

    pattern <- paste0("^", code)
    label   <- EVENT_CODES$LABEL[EVENT_CODES$CODE == code]

    cat(sprintf("Processing diagnosis: %s (%s)", label, code))

    # Load events and keep only the specified codes
    events_raw <- as.data.table(read_parquet(events_file))
    events_raw <- events_raw[grepl(pattern, as.character(CODE), perl = TRUE), .(PATIENT_ID, CODE, DATE)]
    events_raw[, DATE := as.Date(DATE)]

    # Keep the first occurrence per patient × code
    events_raw <- events_raw[events_raw[, .I[which.min(DATE)], by = .(PATIENT_ID, CODE)]$V1]

    # Restrict to cohort doctors and take their earliest event across any matching code
    events_doctors <- events_raw[PATIENT_ID %in% doctor_ids, .(EVENT_DATE = min(DATE)), by = PATIENT_ID]
    setnames(events_doctors, "PATIENT_ID", "DOCTOR_ID")

    # Merge events, outcomes and covariates
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

    # Prepare data for DiD analysis
    df[, ID := as.integer(factor(DOCTOR_ID))]
    df[, G  := fifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)]
    df[, T  := YEAR]

    # For C50, restrict to female doctors only
    if (code == "C50") {
        covariate_formula <- ~ BIRTH_YEAR + SPECIALTY
        df <- df[SEX == 2]
    } else{
        covariate_formula <- ~ BIRTH_YEAR + SPECIALTY + SEX
    }

    n_cases    <- df[EVENT == 1, uniqueN(DOCTOR_ID)]
    n_controls <- df[EVENT == 0, uniqueN(DOCTOR_ID)]
    cat(sprintf("Final cohort: %d doctors (%d cases, %d controls)\n", n_cases + n_controls, n_cases, n_controls))

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
    agg   <- aggte(att, type = "dynamic", na.rm = TRUE)
    results <- data.frame(
        time        = agg$egt, 
        att         = agg$att.egt, 
        se          = agg$se.egt,
        n_cases     = n_cases,
        n_controls  = n_controls
        ) 

    # Save Base DiD long results
    out_long_file <- file.path(outdir, paste0("Supplements_ExtraDiagnosis_", code, "_", TODAY, ".csv"))
    write.csv(results, out_long_file, row.names = FALSE)

}

# Reload all results and combine into a single data frame for plotting
results_list <- list()
DATE <- TODAY # modify if running this script on a different date than the output files were generated

for (code in unique(EVENT_CODES$CODE)) {
    out_long_file <- file.path(outdir, paste0("Supplements_ExtraDiagnosis_", code, "_", DATE, ".csv"))
    results_list[[code]] <- fread(out_long_file)
    results_list[[code]][, `:=`(
        CODE  = code,
        LABEL = EVENT_CODES$LABEL[EVENT_CODES$CODE == code]
    )]
}
results_all <- rbindlist(results_list)

# Plotting
data_plot <- results_all %>%
    filter(time >= -WIN & time <= WIN) %>%
    mutate(
        panel_title = paste0("Results for: ", LABEL, " (ICD10 code: ", CODE, ")"),
        panel_subtitle = paste0("Cases: ", n_cases, ", Controls: ", n_controls)
    )

p <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = "#1f77b4") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~panel_title, ncol = 1, scales = "fixed") +
    labs(
        x = "Years from Event",
        y = "Drop in Prescription Volume\n(compared to controls)"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"))

out_plot_file <- file.path(outdir, paste0("Plot_Supplements_ExtraDiagnosis_all_", TODAY, ".png"))
ggsave(filename = out_plot_file, plot = p, width = 8, height = 12, dpi = 300)