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
    library(metafor)
    library(readr)
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
p <- list()

for (code in code_list) {
    print(code)
    
    events = as.data.table(read_parquet(events_file))
    events[, CODE := as.character(CODE)]
    # extact all codes which are from the same atc class (i.e. start with the same 5 characters) 
    events <- events[startsWith(CODE, substr(code, 1, 5))]
    atc_group_codes = events$CODE %>% unique()

    results = list()

    for (atc_group_code in atc_group_codes) {

        #### Main
        N_THREADS = 10
        setDTthreads(N_THREADS) 

        event_actual_code = atc_group_code
        outcome_code = atc_group_code

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
       
        results[[atc_group_code]] <- df_model %>%
            group_by(YEAR) %>%
            summarise(
                CODE = atc_group_code,
                YEAR = YEAR,
                AVG_N = mean(Ni, na.rm = TRUE),.groups = 'drop'
            ) %>%
            distinct()

    }
    
    # aggregate all resutls and plot togheter
    results_all <- do.call(rbind, results) 
    p[[code]] <- ggplot(results_all, aes(x = YEAR, y = AVG_N, color = CODE)) +
        geom_line() +
        geom_point() +
        labs(title = paste0("Average N of prescriptions for ATC group ", substr(code, 1, 5)), x = "Year", y = "Average N") +
        theme_minimal()
}

# Combine all plots into a single figure
combined_plot <- wrap_plots(p, ncol = 3)
ggsave(filename = paste0(outdir, "Supplements_Nplots", DATE, ".png"), plot = combined_plot, width = 24, height = 16)
