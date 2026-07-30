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

DATE_DATA   <- "20260316"  
TODAY       <- format(Sys.Date(), "%Y%m%d") 

# --- Inputs ---
dataset_file     <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/Results_", DATE_DATA, "/Results_ATC_", DATE_DATA, ".csv")
events_file      <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedEvents_", DATE_DATA, "/processed_events.parquet")
outcomes_file    <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
doctor_list      <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file   <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
renamed_ATC_file <- "/media/volume/Projects/ATC_renamed_codes.csv"

# --- Outputs ---
outdir   <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
outfile1 <- paste0(outdir, "Supplements_DiD_LongitudinalEstimates_", TODAY, ".csv")  
outfile2 <- paste0(outdir, "Supplements_DiD_MetaAnalysisResults_", TODAY, ".csv")    

if (!dir.exists(outdir)) { dir.create(outdir, recursive = TRUE) }


# ============================================================
# 3. Parameters 
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

# -- Empirical Bayes shrinkage --
N_THRESHOLD <- 5  

# -- Pre/post event-time windows used to summarise the event study --
PRE_WINDOW  <- c(-3, -2, -1)  # event-time points averaged for the "before" estimate
POST_WINDOW <- c(1, 2, 3)     # event-time points averaged for the "after" estimate
META_METHOD <- "FE"           # metafor::rma method used to pool estimates within each window

# ============================================================
# 4. Main 
# ============================================================

dataset <- read_csv(dataset_file, show_col_types = FALSE)
dataset <- dataset[dataset$N_CASES >= MIN_CASES, ]

# Apply multiple test correction to the absolute-change p-value
dataset$PVAL_ADJ <- p.adjust(dataset$PVAL_ABS_CHANGE, method = PADJ_METHOD)
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ < SIG_ALPHA

# Apply correction also to the pre- and post-event p-values
dataset$PVAL_PRE_ADJ  <- p.adjust(dataset$PVAL_PRE, method = PADJ_METHOD)
dataset$PVAL_POST_ADJ <- p.adjust(dataset$PVAL_POST, method = PADJ_METHOD)
dataset$SIG_TYPE <- case_when(
    dataset$SIGNIFICANT_CHANGE ~ "Significant",
    TRUE ~ "Not Significant"
)

# Extract list of significant medications to loop over below
code_list <- dataset %>%
    filter(SIG_TYPE == "Significant") %>%
    pull(OUTCOME_CODE) %>%
    unique()

# -----------------------------------------------
# Per-medication longitudinal DiD + meta-analysis
# -----------------------------------------------

for (code in code_list) {

    # use variables as in original DiD medication script
    event_actual_code <- code
    outcome_code <- code

    # ==========================================================
    # STEP 1: Data Loading (handles ATC code renaming)
    # ==========================================================

    # 1. list of doctors, covariates and ATC code renaming file
    doctor_ids  <- fread(doctor_list, header = FALSE)$V1
    covariates  <- fread(covariate_file)
    renamed_ATC <- fread(renamed_ATC_file)

    # 2. events
    events <- as.data.table(read_parquet(events_file))
    events[, CODE := as.character(CODE)]

    # Filter events based on the event code.
    # If the code is an old code that has been modified, exit analysis.
    if (event_actual_code %in% renamed_ATC$ATC_OLD) {
        cat(paste0("Event code ", event_actual_code, " is an old code. Exiting analysis.\n"))
        quit(status = 0)
    }
    # If input code is a new code, keep as is and rename other codes to the new one
    if (event_actual_code %in% renamed_ATC$ATC_NEW) {
        old_codes <- renamed_ATC[ATC_NEW == event_actual_code, ATC_OLD]
        events[CODE %in% old_codes, CODE := event_actual_code]
        cat(paste0("Event code ", event_actual_code, " is a new code. Renaming other codes {", paste(old_codes, collapse = ", "), "} to the new one.\n"))
    }
    events <- events[startsWith(CODE, event_actual_code)]
    event_ids <- unique(events$PATIENT_ID)

    # 3. outcome
    # check if outcome code is a new code that has been renamed, if so load also old codes, rename columns and merge them
    if (outcome_code %in% renamed_ATC$ATC_NEW) {
        outcome_cols1 <- c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes <- as.data.table(read_parquet(outcomes_file, col_select = outcome_cols1))

        old_codes <- unique(renamed_ATC[ATC_NEW == outcome_code, ATC_OLD])
        # Loop through each old code and stack them
        for (old_code in old_codes) {
            outcome_cols2 <- c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", old_code), paste0("Y_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code))
            outcomes2 <- as.data.table(read_parquet(outcomes_file, col_select = outcome_cols2))
            setnames(outcomes2,
                old = c(paste0("N_", old_code), paste0("Y_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code)),
                new = c(paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code)))
            outcomes <- rbind(outcomes, outcomes2)
        }
    } else {
        outcomes_cols <- c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes <- as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))
    }
    outcomes_filtered <- outcomes[DOCTOR_ID %in% doctor_ids]  # QC: only selected doctors

    # ==========================================================
    # STEP 2: Process and merge events, outcomes & covariates
    # ==========================================================

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

    # Prepare covariates
    covariates[, `:=`(
        SPECIALTY = as.character(INTERPRETATION),
        BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))
    )]
    covariates[, `:=`(
        BIRTH_DATE = NULL,
        INTERPRETATION = NULL
    )]

    # Merge covariates
    df_complete <- covariates[df_merged, on = "DOCTOR_ID"]
    df_complete[, `:=`(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]

    # ==========================================================
    # STEP 3: Restrict to buffered prescription timeframe
    # (avoids bias due to medications entering or exiting the market)
    # ==========================================================

    # 1. Calculate original min and max year across all doctors in the cohort
    original_min_year <- min(df_complete[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
    original_max_year <- max(df_complete[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
    # 2. Add buffer to min and max year to avoid bias
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    cat(sprintf("Original range of outcomes: %d-%d | Buffered range of outcomes: %d-%d\n", original_min_year, original_max_year, buffered_min_year, buffered_max_year))
    # Remove all information outside of buffered range
    df_complete <- df_complete[YEAR >= buffered_min_year & YEAR <= buffered_max_year]
    # Exclude events which happened before the first prescription of the outcome / or after the last one (using buffered range)
    df_complete <- df_complete[is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)]

    # ==========================================================
    # STEP 4: Model data preparation
    # ==========================================================

    # Filter out events after pension, and prescriptions after pension
    events_after_pension <- df_complete[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
    df_complete <- df_complete[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]

    # Final model data
    df_model <- as.data.table(df_complete)[
        , `:=`(
            SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
            SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
            Y = get(paste0("Y_", outcome_code)),
            Ni = get(paste0("N_", outcome_code)),
            N = N_general
        )
    ]
    # Replace missing Y values with 0s
    df_model[is.na(Y), Y := 0]

    # Apply empirical Bayes shrinkage
    df_model[, Y_mean := mean(Y[N >= N_THRESHOLD], na.rm = TRUE), by = DOCTOR_ID]
    df_model[, Y := fifelse(
        N < N_THRESHOLD,
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

    # ==========================================================
    # STEP 5: DiD analysis using the 'did' package
    # ==========================================================

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

    agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
    results <- data.frame(
        time = agg_dynamic$egt,
        att  = agg_dynamic$att.egt,
        se   = agg_dynamic$se.egt
    )

    # Save longitudinal results to file: create if first code, append if not
    results$code <- code
    if (!file.exists(outfile1)) {
        write_csv(results, outfile1)
    } else {
        write_csv(results, outfile1, append = TRUE)
    }

    # ==========================================================
    # STEP 6: Pre/post meta-analysis and save summary results
    # ==========================================================

    before_idx <- results$time %in% PRE_WINDOW
    after_idx  <- results$time %in% POST_WINDOW

    # Meta-analysis of pre-period estimates
    pre_data <- data.frame(
        estimate = results$att[before_idx],
        se = results$se[before_idx]
    )
    pre_meta <- metafor::rma(yi = estimate, sei = se, data = pre_data, method = META_METHOD)
    avg_effect_before <- pre_meta$b[, 1]
    se_pre <- pre_meta$se
    p_value_pre <- pre_meta$pval
    ci_pre <- c(pre_meta$ci.lb, pre_meta$ci.ub)

    # Meta-analysis of post-period estimates
    post_data <- data.frame(
        estimate = results$att[after_idx],
        se = results$se[after_idx]
    )
    post_meta <- metafor::rma(yi = estimate, sei = se, data = post_data, method = META_METHOD)
    avg_effect_after <- post_meta$b[, 1]
    se_post <- post_meta$se
    p_value_post <- post_meta$pval
    ci_post <- c(post_meta$ci.lb, post_meta$ci.ub)

    # Absolute change and relative change estimates
    absolute_change <- avg_effect_after - avg_effect_before
    absolute_change_se <- sqrt(se_post^2 + se_pre^2)
    z_score_abs <- absolute_change / absolute_change_se
    abs_change_pval <- 2 * (1 - pnorm(abs(z_score_abs)))

    # Store meta-analysis results in a dataframe and save to file: create if first code, append if not
    meta_results <- data.frame(
        CODE = code,
        ABS_CHANGE = absolute_change,
        SE_ABS_CHANGE = absolute_change_se,
        PVAL_ABS_CHANGE = abs_change_pval,
        EFFECT_PRE = avg_effect_before,
        SE_PRE = se_pre,
        PVAL_PRE = p_value_pre,
        CI_LOW_PRE = ci_pre[1],
        CI_HIGH_PRE = ci_pre[2],
        EFFECT_POST = avg_effect_after,
        SE_POST = se_post,
        PVAL_POST = p_value_post,
        CI_LOW_POST = ci_post[1],
        CI_HIGH_POST = ci_post[2]
    )
    if (!file.exists(outfile2)) {
        write_csv(meta_results, outfile2)
    } else {
        write_csv(meta_results, outfile2, append = TRUE)
    }

}