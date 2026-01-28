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
})

##### Arguments
DATE = "20260123"
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE, '/Results_', DATE, '/Results_ATC_', DATE, '.csv')
events_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedEvents_", DATE, "/processed_events.parquet")
outcomes_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedOutcomes_", DATE, "/processed_outcomes.parquet")
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
renamed_ATC_file = "/media/volume/Projects/ATC_renamed_codes.csv"
outdir = "/media/volume/Projects/DSGELabProject1/Plots/Supplements/"

# caluclate list of FDR significant codes and robust codes 
dataset <- read_csv(dataset_file, show_col_types = FALSE)
dataset <- dataset[dataset$N_CASES >= 300, ]
dataset <- dataset %>%
    mutate(
      Z_SCORE = ABS_CHANGE / ABS_CHANGE_SE,
      PVAL = 2 * (1 - pnorm(abs(Z_SCORE)))
    )
dataset$PVAL_ADJ_FDR <- p.adjust(dataset$PVAL, method = "fdr")
dataset$SIGNIFICANT_FDR <- dataset$PVAL_ADJ_FDR < 0.05
dataset$SIGNIFICANT_ROBUST <- (dataset$PVAL_PRE >= 0.05) & (dataset$PVAL_POST < 0.05)
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_FDR & dataset$SIGNIFICANT_ROBUST ~ "Significant",
  TRUE ~ "Not Significant"
)

# prepare vectors for validation plots
code_list = dataset %>%
    filter(SIGNIFICANT_FDR) %>%
    pull(OUTCOME_CODE) %>%
    unique()
robust_codes = dataset %>%
    filter(SIGNIFICANT_ROBUST) %>%
    pull(OUTCOME_CODE) %>%
    unique()
p <- list()

for (code in code_list) {
    print(code)

    #### Main
    N_THREADS = 10
    setDTthreads(N_THREADS) 

    event_actual_code = code
    outcome_code = code

    # STEP 1: Data Loading 

    # 1. list of doctors, covariates and ATC code renaming file
    doctor_ids = fread(doctor_list, header = FALSE)$V1
    covariates = fread(covariate_file)
    renamed_ATC = fread(renamed_ATC_file)
    # 2. events
    events = as.data.table(read_parquet(events_file))
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
    events <- events[startsWith(CODE, event_actual_code)]
    event_ids <- unique(events$PATIENT_ID)

    # 3. outcome
    # check if outcome code is a new code that has been renamed, if so load also old codes, rename columns and merge them
    if (outcome_code %in% renamed_ATC$ATC_NEW) {
        outcome_cols1 = c("DOCTOR_ID", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcome_cols1))

        old_codes = unique(renamed_ATC[ATC_NEW == outcome_code, ATC_OLD])
        # Loop through each old code and stack them
        for(old_code in old_codes) {
            outcome_cols2 = c("DOCTOR_ID", "YEAR", paste0("N_", old_code), paste0("Y_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code))
            outcomes2 = as.data.table(read_parquet(outcomes_file, col_select = outcome_cols2))     
            setnames(outcomes2, 
                old = c(paste0("N_", old_code), paste0("Y_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code)),
                new = c(paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code)))     
            outcomes = rbind(outcomes, outcomes2)
        }
    } else {
        outcomes_cols = c("DOCTOR_ID", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))
    }
    outcomes_filtered = outcomes[DOCTOR_ID %in% doctor_ids] # QC : only selected doctors

    # STEP 2: Process and merge events, outcomes & covariates

    events = events[, .(PATIENT_ID, CODE, DATE)]
    setnames(events, "PATIENT_ID", "DOCTOR_ID")
    # Keep only the first event per DOCTOR_ID, in case multiple codes exist
    events = events[order(DOCTOR_ID, DATE)]
    events = events[, .SD[1], by = DOCTOR_ID]

    df_merged = events[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
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
    df_complete = covariates[df_merged, on = "DOCTOR_ID"]
    df_complete[, `:=`(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]


    # # STEP 3: Process prescription timeframe to avoid bias due to medications entering or exiting the market

    # 1. Calculate original min and max year across all doctors in the cohort
    original_min_year <- min(df_complete[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
    original_max_year <- max(df_complete[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
    # 2. Add buffer to min and max year to avoid bias
    BUFFER_YEARS = 1
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    cat(sprintf("Original range of outcomes: %d-%d | Buffered range of outcomes: %d-%d\n", original_min_year, original_max_year, buffered_min_year, buffered_max_year))
    # Remove all information outside of buffered range
    df_complete <- df_complete[YEAR >= buffered_min_year & YEAR <= buffered_max_year]
    # Exclude events which happened before the first prescription of the outcome / or after the last one (using buffered range)
    df_complete <- df_complete[is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)]

    # STEP 4: Model Data Preparation

    # Filter out events after pension, and prescriptions after pension
    PENSION_AGE = 60
    events_after_pension = df_complete[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
    df_complete = df_complete[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]
    # final model data
    df_model <- as.data.table(df_complete)[
        , `:=`(
            SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
            SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
            Y = get(paste0("Y_", outcome_code)),
            Ni = get(paste0("N_", outcome_code))
        )
    ]
    # Replace missing Y values with 0s 
    df_model[is.na(Y), Y := 0]
    # prepare variables as requested by did package
    df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                      
    df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)    
    df_model$T <- df_model$YEAR    

    # Calculate number of cases and controls
    n_cases <- length(unique(df_model[df_model$EVENT == 1, DOCTOR_ID]))
    n_controls <- length(unique(df_model[df_model$EVENT == 0, DOCTOR_ID]))
    cat(paste0("Cases: ", n_cases, "\n"))
    cat(paste0("Controls: ", n_controls, "\n"))

    # For cases, count number of unique doctors per event cohorts (i.e., number of events per year)
    events_per_year <- df_model[df_model$EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
    events_year_str <- paste0(events_per_year$EVENT_YEAR, ":", events_per_year$N, collapse = ", ")
    cat("Events per year: [", events_year_str, "]\n")

    # STEP 5: DiD Analysis using 'did' package

    set.seed(09152024)
    att_gt_res <- att_gt(
        yname = "Y",
        tname = "T",
        idname = "ID",
        gname = "G",
        xformla = ~ BIRTH_YEAR + SEX + SPECIALTY,
        data = df_model,
        est_method = "dr",                      # doubly robust (for covariate adj.)
        control_group = "notyettreated",        # use not-yet-treated as control group
        clustervars = "ID",
        pl = TRUE,                              # parallel processing
        cores = N_THREADS
    )

    agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
    results <- data.frame(
        time = agg_dynamic$egt,
        att = agg_dynamic$att.egt,
        se = agg_dynamic$se.egt
    )

    # For medications results will consider ATT and SE in a 3 year window before and after event (t=0)

    # Average effect before event (-3,-2,-1)
    before_idx <- results$time %in% c(-3, -2, -1)
    avg_effect_before <- mean(results$att[before_idx], na.rm = TRUE)
    # Average effect after event (+1,+2,+3)
    after_idx <- results$time %in% c(1, 2, 3)
    avg_effect_after <- mean(results$att[after_idx], na.rm = TRUE)
    # Compute 3-year-window metric, both absolute value and relative value
    absolute_change <- avg_effect_after - avg_effect_before
    relative_change <- ifelse(avg_effect_before == 0, NA, avg_effect_after / avg_effect_before)

    # SE estimation for absolute change
    # NOTE: no vcov matrix returned by did package unfortunately, will assume independence across time points
    se_post <- sqrt(sum(results$se[after_idx]^2)) / 3
    se_pre <- sqrt(sum(results$se[before_idx]^2)) / 3
    absolute_change_se <- sqrt(se_post^2 + se_pre^2)

    # SE estimation for relative change
    # NOTE: no vcov matrix returned by did package unfortunately, will assume independence across time points
    # Delta method for ratio: SE(Y/X) ≈ sqrt((SE_Y/X)^2 + (Y*SE_X/X^2)^2)
    relative_change_se <- sqrt((se_post / avg_effect_before)^2 + (avg_effect_after * se_pre / avg_effect_before^2)^2)

    # Z-score test for pre period (testing if avg_effect_before differs from 0)
    z_stat_pre <- avg_effect_before / se_pre
    p_value_pre <- 2 * (1 - pnorm(abs(z_stat_pre)))

    # Z-score test for post period (testing if avg_effect_after differs from 0)
    z_stat_post <- avg_effect_after / se_post
    p_value_post <- 2 * (1 - pnorm(abs(z_stat_post)))

    # Z-score and p-value for absolute change
    score_abs <- absolute_change / absolute_change_se
    abs_change_pval <- 2 * (1 - pnorm(abs(score_abs)))

    # ============================================================================

    # plot result
    data_plot <- results[results$time >= -3 & results$time <= 3, ]
    subtitle_text <- paste0(
        paste0(
            "N Cases: ", n_cases, " | N Controls: ", n_controls, "\n",
            "Absolute Change: ", round(absolute_change, 5), " | Absolute Change SE: ", round(absolute_change_se, 5), "\n",
            "Relative Change: ", round(relative_change, 5), " | Relative Change SE: ", round(relative_change_se, 5), "\n",
            "p-value (Pre event): ", round(p_value_pre, 5)," | p-value (Post event): ", round(p_value_post, 5), " | p-value (Abs Change): ", round(abs_change_pval, 5)
        )
    )
    
    # Make text bold if code is in robust_codes
    if (code %in% robust_codes) {
        subtitle_text <- paste0("**", subtitle_text, "**")
    }
    
    p[[code]] <- ggplot(data_plot, aes(x = time, y = att)) +
        geom_line(color = "#1f77b4") +
        geom_point(size = 2) +
        geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4", linewidth = 1) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
        labs(
            x = "Years from Event",
            y = "Staggered DiD model Estimate \n(Change in Prescription Rate)",
            title = paste0("medication ATC:", code),
            subtitle = subtitle_text
        ) +
        theme_minimal()
}

# Combine all plots into a single figure
combined_plot <- wrap_plots(p, ncol = 3)
ggsave(filename = paste0(outdir, "Supplementary_RatioDiDValidation_", DATE, ".png"), plot = combined_plot, width = 24, height = 16)
