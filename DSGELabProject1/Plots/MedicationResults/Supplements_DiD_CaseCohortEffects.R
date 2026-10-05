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

DATE_DATA <- "20260920"  
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
PLOT_NCOL   <- 4     # number of panels per row in the combined figure
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

# -- Helper: fill gaps in doctor's follow-up 
fill_gaps_with_0s <- function(dt) {
  
    # 1. Get each doctor's follow-up window
    ranges <- dt[, .(min_year = FOLLOW_UP_START_YEAR[1], max_year = FOLLOW_UP_END_YEAR[1]), by = DOCTOR_ID]
    # 2. Build the year skeleton for each doctor
    skeleton <- ranges[, .(YEAR = seq(min_year, max_year)), by = DOCTOR_ID]
    # 3. Join original data onto the skeleton
    setkey(dt, DOCTOR_ID, YEAR)
    setkey(skeleton, DOCTOR_ID, YEAR)
    filled <- dt[skeleton]

    # 4. Zero-fill missing values (N and Y columns)
    filled[, N := fifelse(is.na(N), 0, N)]
    filled[, Ni := fifelse(is.na(Ni), 0, Ni)]
    filled[, Y := fifelse(is.na(Y), 0, Y)]
    # 5. Carry forward "fixed" covariate columns, but not AGE
    fixed_cols <- setdiff(names(dt), c("DOCTOR_ID", "YEAR", "N", "Ni", "Y", "AGE"))
    if (length(fixed_cols) > 0) {
      filled[, (fixed_cols) := lapply(.SD, function(x) {
          val <- x[!is.na(x)][1]
          fifelse(is.na(x), val, x)
      }), .SDcols = fixed_cols, by = DOCTOR_ID]
    }
    # 6. Recompute AGE for every row
    filled[, AGE := YEAR - BIRTH_YEAR]  

  return(filled)
}

# ============================================================
# 4. Global settings
# ============================================================


# -- General analysis parameters --
MIN_CASES   <- 300           
PADJ_METHOD <- "bonferroni"  
SIG_ALPHA   <- 0.05          

# -- Compute / data.table parameters --
N_THREADS <- 10  
setDTthreads(N_THREADS)

# -- Data cleaning / eligibility windows --
BUFFER_YEARS   <- 1   
PENSION_AGE    <- 60     
  
# Medications of interest: ATC code -> readable label
code_labels <- tibble(
    OUTCOME_CODE = c(
        "C10AA07",
        "J01FA09",
        "M01AH05",
        "N02BE01",
        "N05CF02",
        "R01AD12",
        "R01AD58"
    ),
    LABEL = c(
        "rosuvastatin",
        "clarithromycin",
        "etoricoxib",
        "paracetamol",
        "zolpidem",
        "fluticasone furoate",
        "fluticasone, combinations"
    )
)


# ============================================================
# 5. Load the main results table and identify significant medications
# ============================================================

main_results <- read_csv(PATH_MAIN_RESULTS, show_col_types = FALSE)
main_results <- main_results[main_results$N_CASES >= MIN_CASES, ]

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
    SPECIALTY = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    LICENSE_START = as.Date(START_DATE),
    LICENSE_END   = as.Date(END_DATE),
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

    # Filter events based on the event code
    # If the code is an old code that have been modified, exit analysis
    if (event_actual_code %in% renamed_ATC$ATC_OLD) {
        cat(paste0("Event code ", event_actual_code, " is an old code. Exiting analysis.\n"))
        quit(status = 0)
    }
    # If input code is a new code, keep as is and rename other codes to the new one
    if (event_actual_code %in% renamed_ATC$ATC_NEW) {
        old_codes = renamed_ATC[ATC_NEW == event_actual_code, ATC_OLD]
        events[CODE %in% old_codes, CODE := event_actual_code]
        cat(paste0("Event code ", event_actual_code, " is a new code. Renaming other codes {", paste(old_codes, collapse = ", "), "} to the new one.\n"))
    }
    # Extract events, and in case multiple exist keep only the first one
    events <- events[startsWith(CODE, event_actual_code)]
    setorder(events, PATIENT_ID, DATE)
    events = events[, .SD[1], by = .(PATIENT_ID)]
    event_ids <- unique(events$PATIENT_ID)

    # ------------------------------------------------------------
    # 6b. Load outcomes, stacking old codes into the new one if renamed
    # ------------------------------------------------------------

    # check if outcome code is a new code that has been renamed, if so load also old codes, rename columns and merge them
    outcome_N_col     = paste0("N_", outcome_code)
    outcome_Y_col     = paste0("Y_", outcome_code)
    outcome_first_col = paste0("first_year_", outcome_code)
    outcome_last_col  = paste0("last_year_", outcome_code)

    if (outcome_code %in% renamed_ATC$ATC_NEW) {
        outcome_cols1 = c("DOCTOR_ID", "YEAR", "N_general", outcome_N_col, outcome_first_col, outcome_last_col)
        outcomes = as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = outcome_cols1))
        # ensure numeric cols are double, not int, before rbind (parquet columns can be typed differently per source)
        num_cols1 = c("N_general", outcome_N_col, outcome_first_col, outcome_last_col)
        outcomes[, (num_cols1) := lapply(.SD, as.double), .SDcols = num_cols1]

        old_codes = unique(renamed_ATC[ATC_NEW == outcome_code, ATC_OLD])
        # Loop through each old code, rename its columns to match the new code, and stack
        for(old_code in old_codes) {
            outcome_cols2 = c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code))
            outcomes2 = as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = outcome_cols2))     
            # ensure numeric cols are double, not int, before rbind (parquet columns can be typed differently per source)
            num_cols2 = c("N_general", paste0("N_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code))
            outcomes2[, (num_cols2) := lapply(.SD, as.double), .SDcols = num_cols2]
            setnames(outcomes2, 
                old = c(paste0("N_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code)),
                new = c(outcome_N_col, outcome_first_col, outcome_last_col))     
            outcomes = rbind(outcomes, outcomes2)
        }

        # Collapse the multiple medication rows into a single row per DOCTOR_ID/YEAR:
        outcomes = outcomes[, .(
            N_general = N_general[1], # N_general is the same for all rows of the same doctor/year
            NEW_N     = sum(get(outcome_N_col), na.rm = TRUE),
            NEW_FIRST = min(get(outcome_first_col), na.rm = TRUE),
            NEW_LAST  = max(get(outcome_last_col), na.rm = TRUE)
        ), by = .(DOCTOR_ID, YEAR)]

        # Convert Inf/-Inf to NA in first/last year 
        outcomes[is.infinite(NEW_FIRST), NEW_FIRST := NA_real_]
        outcomes[is.infinite(NEW_LAST),  NEW_LAST  := NA_real_]

        # Calculate the final medication ratio value (Y)
        outcomes[, NEW_Y := fifelse(N_general > 0, NEW_N / N_general, NA_real_)]

        # Rename the columns to match the original format
        setnames(outcomes,
            old = c("NEW_N", "NEW_Y", "NEW_FIRST", "NEW_LAST"),
            new = c(outcome_N_col, outcome_Y_col, outcome_first_col, outcome_last_col))
    } else {
        outcomes_cols = c("DOCTOR_ID", "YEAR", "N_general", outcome_N_col, outcome_Y_col, outcome_first_col, outcome_last_col)
        outcomes = as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = outcomes_cols))
    }
    outcomes_filtered = outcomes[DOCTOR_ID %in% doctor_ids] # QC : only selected doctors

    # ------------------------------------------------------------
    # 6c. Merge events, outcomes and covariates
    # ------------------------------------------------------------

    events <- events[, .(PATIENT_ID, CODE, DATE)]
    setnames(events, "PATIENT_ID", "DOCTOR_ID")
    # Keep only the first event per DOCTOR_ID, in case multiple codes exist
    events <- events[order(DOCTOR_ID, DATE)]
    events <- events[, .SD[1], by = DOCTOR_ID]

    df_merged <- events[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
    df_merged[, DATE := as.Date(DATE)]
    df_merged[, EVENT := ifelse(!is.na(DATE), 1, 0)]
    df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
    df_merged[, DATE := NULL]

    # Merge covariates
    df_complete <- covariates[df_merged, on = "DOCTOR_ID"]
    df_complete[, `:=`(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]
    # ------------------------------------------------------------
    # 6d. Trim the medication's on-market window 
    #     (avoid bias from the drug entering/exiting the market during the study period)
    # ------------------------------------------------------------

    # 1. Calculate original min and max year across all doctors in the cohort
    original_min_year <- min(df_complete[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
    original_max_year <- max(df_complete[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
    # 2. Add buffer to min and max year to avoid bias
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    cat(sprintf("Original range of outcomes: %d-%d | Buffered range of outcomes: %d-%d\n", original_min_year, original_max_year, buffered_min_year, buffered_max_year))
    # Exclude events which happened before the first prescription of the outcome / or after the last one (using buffered range)
    df_complete <- df_complete[is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)]

    # ------------------------------------------------------------
    # 6e. Model data preparation
    # ------------------------------------------------------------

    # Filter out events after pension, and prescriptions after pension
    events_after_pension = df_complete[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
    df_complete = df_complete[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]

    # final model data
    df_model <- as.data.table(df_complete)[
        , `:=`(
            SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
            SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
            Y = get(paste0("Y_", outcome_code)),
            Ni = get(paste0("N_", outcome_code)),
            N = N_general
        )
    ]

    # Replace missing values within follow-up with 0s 
    df_model[, `:=`(
        FOLLOW_UP_START = pmax(as.Date("1998-01-01"), LICENSE_START, na.rm = TRUE),
        FOLLOW_UP_END   = pmin(as.Date("2022-12-31"), LICENSE_END, BIRTH_DATE + 60 * 365.25, DEATH_DATE, na.rm = TRUE)
    )]
    df_model[, `:=`(
        FOLLOW_UP_START_YEAR = as.integer(format(FOLLOW_UP_START, "%Y")),
        FOLLOW_UP_END_YEAR   = as.integer(format(FOLLOW_UP_END, "%Y"))
    )]
    df_model = fill_gaps_with_0s(df_model)

    # Remove all information outside of buffered market range
    df_model <- df_model[YEAR >= buffered_min_year & YEAR <= buffered_max_year]

    # To ensure results are robust will apply "empirical bayes shrinkage" to doctors with low total prescriptions in a given year
    # Will shrink the ratio toward the mean from years with N >= N_THRESHOLD; if none qualify, will use all years 
    N_THRESHOLD = 5
    df_model[, Y_mean := {
        eligible = (N >= N_THRESHOLD)
        if (any(eligible)) {mean(Y[eligible], na.rm = TRUE)} 
        else {mean(Y, na.rm = TRUE)}
    }, by = DOCTOR_ID]
    # Apply empirical Bayes shrinkage: adjust Y values where N < N_THRESHOLD
    df_model[, Y := fifelse(
        (N != 0) & (N < N_THRESHOLD), 
        ((N * Y + N_THRESHOLD * Y_mean) / (N + N_THRESHOLD)), 
        Y
    )]
    df_model[, Y_mean := NULL]

    # Prepare variables as required by the 'did' package
    df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))
    df_model$G  <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)
    df_model$T  <- df_model$YEAR

    # Calculate number of cases and controls
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