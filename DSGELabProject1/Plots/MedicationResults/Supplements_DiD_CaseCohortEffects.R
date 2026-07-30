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

# --- Input ---
PATH_MAIN_RESULTS    <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/Results_", DATE_DATA, "/Results_ATC_", DATE_DATA, ".csv")
PATH_EVENTS_FILE     <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedEvents_", DATE_DATA, "/processed_events.parquet")
PATH_OUTCOMES_FILE   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
PATH_DOCTOR_LIST     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
PATH_COVARIATES_FILE <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
PATH_RENAMED_ATC     <- "/media/volume/Projects/ATC_renamed_codes.csv"

# --- Output ---
DIR_OUT <- paste0("/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/")
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)

FILE_RESULTS_CSV   <- paste0("Supplements_DiD_CaseCohortEffects_Results_", TODAY, ".csv")
FILE_PLOT_BASENAME <- paste0("Supplements_DiD_CaseCohortEffects_Plot_", TODAY)

# ============================================================
# 3. Plotting parameters 
# ============================================================

# -- Export settings --
PLOT_DPI    <- 300
PLOT_NCOL   <- 5     # number of panels per row in the combined figure
PLOT_WIDTH  <- 30
PLOT_HEIGHT <- 15

# -- Colors / theme --
COLOR_LINE      <- "#2ca02c"
COLOR_ZERO_LINE <- "grey"
THEME_BASE      <- theme_minimal()

# -- Helper: save a ggplot as both PNG and PDF using the same base filename --
save_plot_png_pdf <- function(plot, dir, basename, width, height, dpi = PLOT_DPI) {
    ggsave(filename = file.path(dir, paste0(basename, ".png")), 
        plot = plot,
        width = width, 
        height = height, 
        dpi = dpi)
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

MIN_N_CASES  <- 300         # minimum cases required for a medication to be considered
PVAL_METHOD  <- "bonferroni"
ALPHA        <- 0.05

# Market entrance/exit buffer years
BUFFER_YEARS <- 1  

# Age threshold for pension (doctors older than this age are excluded from the analysis)
PENSION_AGE  <- 60          

# empirical Bayes shrinkage threshold
N_THRESHOLD <- 5    

# Medications of interest: ATC code -> readable label
code_labels <- tibble(
    OUTCOME_CODE = c(
        "A06AC01",
        "C10AA07",
        "M01AH05",
        "N02CC07",
        "N05CF02",
        "N06AX26",
        "R01AD12",
        "R01AD58",
        "R03AK10"
    ),
    LABEL = c(
        "Ispaghula (psylla seeds)",
        "Rosuvastatin",
        "Etoricoxib",
        "Frovatriptan",
        "Zolpidem",
        "Vortioxetine",
        "Fluticasone furoate",
        "Fluticasone, combinations",
        "Vilanterol and fluticasone furoate"
    )
)


# ============================================================
# 5. Load the main results table and identify significant medications
# ============================================================

main_results <- read_csv(PATH_MAIN_RESULTS, show_col_types = FALSE)
main_results <- main_results[main_results$N_CASES >= MIN_N_CASES, ]

# Apply multiple test correction
main_results$PVAL_ADJ <- p.adjust(main_results$PVAL_ABS_CHANGE, method = PVAL_METHOD)
main_results$SIGNIFICANT_CHANGE <- main_results$PVAL_ADJ < ALPHA
main_results$SIG_TYPE <- case_when(
    main_results$SIGNIFICANT_CHANGE ~ "Significant",
    TRUE ~ "Not Significant"
)

# Extract list of significant medications to re-analyze/plot below
code_list <- main_results %>%
    filter(SIG_TYPE == "Significant") %>%
    pull(OUTCOME_CODE) %>%
    unique()

cat(sprintf("Significant medications to process: %d\n", length(code_list)))


# --- Load shared reference data ----
doctor_ids  <- fread(PATH_DOCTOR_LIST, header = FALSE)$V1
covariates  <- fread(PATH_COVARIATES_FILE)
renamed_ATC <- fread(PATH_RENAMED_ATC)

covariates[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]

# ============================================================
# 6. Per-medication pipeline
# ============================================================

results_list <- list()
for (code in code_list) {

    # use variables as in the original single-medication DiD script
    event_actual_code <- code
    outcome_code      <- code

    # ------------------------------------------------------------
    # 6a. Load events, resolving any ATC code renaming
    # ------------------------------------------------------------

    events <- as.data.table(read_parquet(PATH_EVENTS_FILE))
    events[, CODE := as.character(CODE)]

    # If the code is an OLD code that has since been renamed, skip it
    if (event_actual_code %in% renamed_ATC$ATC_OLD) {
        cat(paste0("Event code ", event_actual_code, " is an old code. Skipping.\n"))
        next
    }
    # If input code is a NEW code, keep as is and rename any old codes to the new one
    if (event_actual_code %in% renamed_ATC$ATC_NEW) {
        old_codes <- renamed_ATC[ATC_NEW == event_actual_code, ATC_OLD]
        events[CODE %in% old_codes, CODE := event_actual_code]
        cat(paste0("Event code ", event_actual_code, " is a new code. Renaming other codes {", paste(old_codes, collapse = ", "), "} to the new one.\n"))
    }
    events <- events[startsWith(CODE, event_actual_code)]

    # ------------------------------------------------------------
    # 6b. Load outcomes, stacking old codes into the new one if renamed
    # ------------------------------------------------------------

    if (outcome_code %in% renamed_ATC$ATC_NEW) {
        outcome_cols <- c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes <- as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = outcome_cols))

        old_codes <- unique(renamed_ATC[ATC_NEW == outcome_code, ATC_OLD])
        for (old_code in old_codes) {
            outcome_cols_old <- c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", old_code), paste0("Y_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code))
            outcomes_old <- as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = outcome_cols_old))
            setnames(outcomes_old,
                old = c(paste0("N_", old_code), paste0("Y_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code)),
                new = c(paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code)))
            outcomes <- rbind(outcomes, outcomes_old)
        }
    } else {
        outcome_cols <- c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes <- as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = outcome_cols))
    }
    outcomes_filtered <- outcomes[DOCTOR_ID %in% doctor_ids]   # QC: only selected doctors

    # ------------------------------------------------------------
    # 6c. Merge events, outcomes and covariates
    # ------------------------------------------------------------

    events <- events[, .(PATIENT_ID, CODE, DATE)]
    setnames(events, "PATIENT_ID", "DOCTOR_ID")
    # Keep only the first event per doctor, in case multiple matching codes exist
    events <- events[order(DOCTOR_ID, DATE)]
    events <- events[, .SD[1], by = DOCTOR_ID]

    df_merged <- events[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
    df_merged[, DATE := as.Date(DATE)]
    df_merged[, EVENT := ifelse(!is.na(DATE), 1, 0)]
    df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
    df_merged[, DATE := NULL]

    df_complete <- covariates[df_merged, on = "DOCTOR_ID"]
    df_complete[, `:=`(
        AGE          = YEAR - BIRTH_YEAR,
        AGE_IN_2023  = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]

    # ------------------------------------------------------------
    # 6d. Trim the medication's on-market window 
    #     (avoid bias from the drug entering/exiting the market during the study period)
    # ------------------------------------------------------------

    original_min_year <- min(df_complete[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
    original_max_year <- max(df_complete[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    cat(sprintf("Original range of outcomes: %d-%d | Buffered range of outcomes: %d-%d\n",
                original_min_year, original_max_year, buffered_min_year, buffered_max_year))

    df_complete <- df_complete[YEAR >= buffered_min_year & YEAR <= buffered_max_year]
    # Exclude events which happened before the first prescription of the outcome, or after the last one
    df_complete <- df_complete[is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)]

    # ------------------------------------------------------------
    # 6e. Model data preparation
    # ------------------------------------------------------------

    # Remove doctors whose event happened after pension, and prescriptions logged after pension
    events_after_pension <- df_complete[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
    df_complete <- df_complete[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]

    df_model <- as.data.table(df_complete)[
        , `:=`(
            SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
            SEX       = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
            Y         = get(paste0("Y_", outcome_code)),
            Ni        = get(paste0("N_", outcome_code)),
            N         = N_general
        )
    ]
    # Replace missing Y (prescription ratio) values with 0s
    df_model[is.na(Y), Y := 0]

    # Empirical Bayes shrinkage
    df_model[, Y_mean := mean(Y[N >= N_THRESHOLD], na.rm = TRUE), by = DOCTOR_ID]
    df_model[, Y := fifelse(
        N < N_THRESHOLD,
        ((N * Y + N_THRESHOLD * Y_mean) / (N + N_THRESHOLD)),
        Y
    )]
    df_model[, Y_mean := NULL]

    # DiD variables: numeric ID, group (first treatment year), calendar year
    df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))
    df_model$G  <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)
    df_model$T  <- df_model$YEAR

    n_cases    <- length(unique(df_model[df_model$EVENT == 1, DOCTOR_ID]))
    n_controls <- length(unique(df_model[df_model$EVENT == 0, DOCTOR_ID]))

    # ------------------------------------------------------------
    # 6f. DiD model, aggregated by treatment-year cohort 
    # ------------------------------------------------------------

    set.seed(09152024)
    att_gt_res <- att_gt(
        yname = "Y",
        tname = "T",
        idname = "ID",
        gname = "G",
        xformla = ~ BIRTH_YEAR + SEX + SPECIALTY,
        data = df_model,
        est_method = "dr",                 # doubly robust (for covariate adjustment)
        control_group = "notyettreated",   # use not-yet-treated as control group
        clustervars = "ID",
        pl = TRUE,
        cores = N_THREADS
    )

    # "group" effect (time of event/cohort) instead of "dynamic" (time from event)
    agg_group <- aggte(att_gt_res, type = "group", na.rm = TRUE)
    results <- data.frame(
        code        = code,
        n_cases     = n_cases,
        n_controls  = n_controls,
        time        = agg_group$egt,
        att         = agg_group$att.egt,
        se          = agg_group$se.egt
    )
    results_list[[code]] <- results
}


# ============================================================
# 7. Combine results 
# ============================================================

# combined data
all_results <- rbindlist(results_list)
write_csv(all_results, file.path(DIR_OUT, FILE_RESULTS_CSV))

# ============================================================
# 8. Combined Plot
# ============================================================

# Join readable labels onto the results
all_results <- all_results %>%
    left_join(code_labels, by = c("code" = "OUTCOME_CODE")) %>%
    mutate(med_label = ifelse(!is.na(LABEL), LABEL, code), LABEL = NULL)

# for each code in all_results
ymin_limit <- -0.035
ymax_limit <- 0.035

# Order codes to match code_labels (same order as the specialty script)
code_order <- code_labels$OUTCOME_CODE[code_labels$OUTCOME_CODE %in% unique(all_results$code)]

plot_list <- list()
for (curr_code in code_order) {

    results_i   <- subset(all_results, code == curr_code)
    n_cases     <- unique(results_i$n_cases)
    n_controls  <- unique(results_i$n_controls)
    med_label   <- unique(results_i$med_label)
    subtitle_text <- paste0("N Cases: ", n_cases, " | N Controls: ", n_controls, "\n")

    # Trim CI bounds to limits and add arrow indicators
    ci_lower_full <- results_i$att - 1.96 * results_i$se
    ci_upper_full <- results_i$att + 1.96 * results_i$se
    results_i$ci_lower <- pmax(ci_lower_full, ymin_limit)
    results_i$ci_upper <- pmin(ci_upper_full, ymax_limit)
    results_i$arrow_lower <- ci_lower_full < ymin_limit
    results_i$arrow_upper <- ci_upper_full > ymax_limit
    results_i$arrow_len <- 0.0015

    plot_list[[curr_code]] <- ggplot(results_i, aes(x = time, y = att)) +
        geom_line(color = COLOR_LINE) +
        geom_point() +
        geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = COLOR_LINE) +
        geom_segment(
            data = subset(results_i, arrow_lower),
            aes(x = time, xend = time, y = ymin_limit + arrow_len, yend = ymin_limit),
            color = COLOR_LINE,
            linewidth = 0.4,
            arrow = arrow(length = grid::unit(0.11, "inches"), type = "closed")
        ) +
        geom_segment(
            data = subset(results_i, arrow_upper),
            aes(x = time, xend = time, y = ymax_limit - arrow_len, yend = ymax_limit),
            color = COLOR_LINE,
            linewidth = 0.4,
            arrow = arrow(length = grid::unit(0.11, "inches"), type = "closed")
        ) +
        geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_ZERO_LINE) +
        coord_cartesian(ylim = c(ymin_limit, ymax_limit)) +
        labs(
            title    = med_label,
            subtitle = subtitle_text,
            x        = "Event Year \n(Case Cohort)",
            y        = "ATT Estimate \n(within each case cohort)"
        ) +
        THEME_BASE
}
        
# combined plot
combined_plot <- wrap_plots(plot_list, ncol = PLOT_NCOL)
save_plot_png_pdf(combined_plot, DIR_OUT, FILE_PLOT_BASENAME, PLOT_WIDTH, PLOT_HEIGHT)