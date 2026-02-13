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
    library(readr)
    library(metafor)
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

# prepare vectors for validation plots
code_list = dataset %>%
    filter(SIG_TYPE == "Significant") %>%
    pull(OUTCOME_CODE) %>%
    unique()
result_list_1 = list()

for (code in code_list) {
    tryCatch({
        event_code = paste0('Purch_', code)
        outcome_code = code

        #### Main
        N_THREADS = 10
        setDTthreads(N_THREADS) 
        options(datatable.verbose = FALSE)
        # not using all threads to easily run in background

        # STEP 1: Data Loading 
        covariates = fread(covariate_file)
        # Prepare covariates 
        covariates[, `:=`(
            SPECIALTY = as.character(INTERPRETATION),
            BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))
        )]
        covariates[, `:=`(
            BIRTH_DATE = NULL, 
            INTERPRETATION = NULL)
        ]
        doctor_ids = fread(doctor_list, header = FALSE)$V1

        events = as.data.table(read_parquet(events_file))
        event_code_parts = strsplit(event_code, "_")[[1]]
        event_source = event_code_parts[1]
        event_actual_code = event_code_parts[2]

        # Filter events based on the event code
        events = events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
        event_ids = unique(events$PATIENT_ID)

        # Load outcomes (N, Ni, and Y for desired medication)
        outcomes_cols = c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))

        # STEP 2: Data Preparation
        # Process and merge events and outcomes

        events_new = events[, .(PATIENT_ID, CODE, DATE)]
        events_new = events_new[CODE == outcome_code]
        setnames(events_new, "PATIENT_ID", "DOCTOR_ID")
        # QC: Keep only the first event per DOCTOR_ID, in case multiple codes exist
        events_new = events_new[order(DOCTOR_ID, DATE)]
        events_new = events_new[, .SD[1], by = DOCTOR_ID]
        # QC: Ensure events are only for doctors in the doctor list
        outcomes_filtered = outcomes[DOCTOR_ID %in% doctor_ids]

        df_merged = events_new[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
        df_merged[, DATE := as.Date(DATE)]
        df_merged[, EVENT := ifelse(!is.na(DATE), 1, 0)]
        df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
        df_merged[, DATE := NULL]

        # Select only events that happened after 2010
        df_merged = df_merged[is.na(EVENT_YEAR) | EVENT_YEAR >= 2011]

        # Merge covariates
        df_complete = covariates[df_merged, on = "DOCTOR_ID"]
        df_complete[, `:=`(
            AGE = YEAR - BIRTH_YEAR,
            AGE_IN_2023 = 2023 - BIRTH_YEAR,
            AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
        )]

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
                Ni = get(paste0("N_", outcome_code)),
                N = N_general
            )
        ]
        # Replace missing Y values with 0s 
        df_model[is.na(Y), Y := 0]

        # STEP 3: Calculate prescription tier based on pre-event prescriptions
        # For controls and cases, calculate mean prescriptions before event
        df_model[, prescription_tier := NA_character_]
        df_model[, prescription_tier_value := {
            event_year <- EVENT_YEAR[1]
            if (!is.na(event_year)) {
            round(mean(Ni[YEAR < event_year], na.rm = TRUE), 0)
            } else {
            round(mean(Ni, na.rm = TRUE), 0)
            }
        }, by = DOCTOR_ID]

        # Calculate percentiles (for both cases and controls)
        p10 <- quantile(df_model$prescription_tier_value, probs = 0.10, na.rm = TRUE)
        p90 <- quantile(df_model$prescription_tier_value, probs = 0.90, na.rm = TRUE)

        # Create tier categories (for both cases and controls)
        df_model[, prescription_tier := fcase(
            prescription_tier_value <= p10, paste0("Low (0-", floor(p10), ")"),
            prescription_tier_value >= p90, paste0("High (", floor(p90) + 1, "+)"),
            default = NA_character_
        )]

        # Convert prescription_tier to a factor with ordered levels
        df_model[, prescription_tier := factor(prescription_tier, levels = c(
            paste0("Low (0-", floor(p10), ")"),
            paste0("High (", floor(p90) + 1, "+)")
        ))]

        # Report statistics on doctors per tier
        tier_stats <- df_model[, .(n_doctors = uniqueN(DOCTOR_ID)), by = prescription_tier][order(prescription_tier)]
        cat("\nDoctors per prescription tier:\n")
        print(tier_stats)

        # Stratified analysis by tier
        result_list_2 <- list()
        tiers <- levels(df_model$prescription_tier)[!is.na(levels(df_model$prescription_tier))]

        for (tier in tiers) {
            df_tier <- df_model[prescription_tier == tier | is.na(prescription_tier)]

            # Keep only doctors who have N >= 5 for ALL years they appear in the data
            docs_to_keep <- df_tier[, .(all_years_ok = all(N >= 5)), by = DOCTOR_ID][all_years_ok == TRUE, DOCTOR_ID]
            df_tier <- df_tier[DOCTOR_ID %in% docs_to_keep]
            
            n_cases_tier <- length(unique(df_tier[!is.na(EVENT_YEAR), DOCTOR_ID]))
            n_controls_tier <- length(unique(df_tier[is.na(EVENT_YEAR), DOCTOR_ID]))

            # prepare variables as requested by did package
            df_tier$ID <- as.integer(factor(df_tier$DOCTOR_ID))                      
            df_tier$G <- ifelse(is.na(df_tier$EVENT_YEAR), 0, df_tier$EVENT_YEAR)    
            df_tier$T <- df_tier$YEAR    
            
            att_gt_res_tier <- att_gt(
                yname = "Y",
                tname = "T",
                idname = "ID",
                gname = "G",
                xformla = ~ BIRTH_YEAR + SEX + SPECIALTY,
                data = df_tier,
                est_method = "dr",
                control_group = "notyettreated",
                clustervars = "ID",
                pl = TRUE,
                cores = N_THREADS
            )
            
            agg_dynamic <- aggte(att_gt_res_tier, type = "dynamic", na.rm = TRUE)
            results <- data.frame(
                time = agg_dynamic$egt,
                att = agg_dynamic$att.egt,
                se = agg_dynamic$se.egt
            )

            # For medications results will consider ATT and SE in a 3 year window before and after event (t=0)
            before_idx <- results$time %in% c(-3, -2, -1)
            after_idx <- results$time %in% c(1, 2, 3)

            # Meta-analysis of pre-period estimates
            pre_data <- data.frame(
                estimate = results$att[before_idx],
                se = results$se[before_idx]
            )
            pre_meta <- metafor::rma(yi = estimate, sei = se, data = pre_data, method = "FE")
            avg_effect_before <- pre_meta$b[,1]
            se_pre <- pre_meta$se
            p_value_pre <- pre_meta$pval

            # Meta-analysis of post-period estimates
            post_data <- data.frame(
                estimate = results$att[after_idx],
                se = results$se[after_idx]
            )
            post_meta <- metafor::rma(yi = estimate, sei = se, data = post_data, method = "FE")
            avg_effect_after <- post_meta$b[,1]
            se_post <- post_meta$se
            p_value_post <- post_meta$pval

            # Absolute change and relative change estimates
            absolute_change <- avg_effect_after - avg_effect_before
            absolute_change_se <- sqrt(se_post^2 + se_pre^2)
            score_abs <- absolute_change / absolute_change_se
            p_value_change <- 2 * (1 - pnorm(abs(score_abs)))


            result_list_2[[tier]] <- data.frame(
                prescription_tier = tier,
                n_cases = n_cases_tier,
                n_controls = n_controls_tier,
                absolute_change = round(absolute_change, 5),
                absolute_change_se = round(absolute_change_se, 5),
                p_value_change = round(p_value_change, 5)
            )
        }

        # combine results and save
        result_df <- do.call(rbind, result_list_2)
        result_df$code <- code
        write.csv(result_df, file.path(outdir, paste0("TempData/Results_PrescriptionTiers_", code, ".csv")), row.names = FALSE)
        result_list_1[[code]] <- result_df

    }, error = function(e) {
    result_list_1[[code]] <- NULL
  })
}

# join all data into final table
# Combine all results into one data frame
combined_results <- do.call(rbind, result_list_1)
rownames(combined_results) <- NULL

# Extract and simplify tier information
combined_results <- combined_results %>%
    mutate(
        tier_name = gsub("\\s*\\([^)]*\\)", "", prescription_tier),
        tier_range = gsub(".*\\((.*)\\).*", "\\1", prescription_tier)
    )

# Make results wide: pivot so each row is a code and columns are tier-specific values
results_wide <- combined_results %>%
    pivot_wider(
        id_cols = code,
        names_from = tier_name,
        values_from = c(absolute_change, absolute_change_se, p_value_change, tier_range),
        names_glue = "{tier_name}_{.value}"
    )

# Test if absolute changes are significantly different between tiers using z-test
results_wide$tier_significance <- apply(results_wide, 1, function(row) {
    # Extract estimates and SEs for Low and High tiers
    low_est <- as.numeric(row["Low_absolute_change"])
    low_se <- as.numeric(row["Low_absolute_change_se"])
    high_est <- as.numeric(row["High_absolute_change"])
    high_se <- as.numeric(row["High_absolute_change_se"])
    
    # Perform z-test if both estimates are available
    if (!is.na(low_est) && !is.na(high_est) && !is.na(low_se) && !is.na(high_se)) {
        # Calculate z-statistic: (est1 - est2) / sqrt(se1^2 + se2^2)
        z_stat <- (low_est - high_est) / sqrt(low_se^2 + high_se^2)
        # Two-tailed p-value
        p_value <- 2 * (1 - pnorm(abs(z_stat)))
        
        # Return star system
        return(p_value)
    }
    return(NA)
})

# Save final results
write.csv(results_wide, file.path(outdir, "Results_PrescriptionTiers_Final.csv"), row.names = FALSE)
