# ==============================================================================
# 0. LIBRARIES
# ==============================================================================

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
    library(readr)
    library(metafor)
})


# ==============================================================================
# 1. PATHS
# ==============================================================================

DATE_DATA <- "20260920"
TODAY     <- format(Sys.Date(), "%Y%m%d")

# --- Inputs ---
dataset_file     <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE_DATA, '/Results_', DATE_DATA, '/Results_ATC_', DATE_DATA, '.csv')
events_file      <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedEvents_", DATE_DATA, "/processed_events.parquet")
outcomes_file    <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
doctor_list      <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file   <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# --- Output ---
outdir  <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(outdir)) { dir.create(outdir, recursive = TRUE) }

outfile <- paste0(outdir, "Supplements_StratifiedAnalysis_Age_", TODAY, ".csv")
# no plots are generated for this analysis

# ==============================================================================
# 2. PARAMETERS / ARGUMENTS
# ==============================================================================

N_THREADS      <- 10    
setDTthreads(N_THREADS)

# --- Filtering / significance thresholds ---
MIN_CASES   <- 300
PADJ_METHOD <- "bonferroni"
SIG_ALPHA   <- 0.05

# --- Cohort construction ---
BUFFER_YEARS   <- 1     # market entrance/exit buffer years
PENSION_AGE    <- 60    

# --- Age-tier definition (fixed birth-year cutoffs) ---
LOW_BEFORE_YEAR <- 1972   # Low tier: born before this year
HIGH_AFTER_YEAR <- 1972   # High tier: born after this year

# --- Event-time windows used for the pre/post fixed-effects meta-analysis ---
PRE_WINDOW  <- c(-3, -2, -1)
POST_WINDOW <- c(1, 2, 3)
META_METHOD <- "FE"

#### Functions:
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

# ==============================================================================
# 3. MAIN
# ==============================================================================

dataset <- read_csv(dataset_file, show_col_types = FALSE)
dataset <- dataset[dataset$N_CASES >= MIN_CASES, ]

# Apply multiple test correction
dataset$PVAL_ADJ <- p.adjust(dataset$PVAL_ABS_CHANGE, method = PADJ_METHOD)
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ < SIG_ALPHA
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_CHANGE ~ "Significant",
  TRUE ~ "Not Significant"
)

# Extract list of significant medications for plots
code_list <- dataset %>%
    filter(SIG_TYPE == "Significant") %>%
    pull(OUTCOME_CODE) %>%
    unique()


# ==============================================================================
# 4. PER-MEDICATION, PER-AGE-TIER DiD PIPELINE
# ==============================================================================

result_list_1 <- list()

for (code in code_list) {
    tryCatch({
        event_code   <- paste0('Purch_', code)
        outcome_code <- code

        # --- STEP 1: Data loading ---
        doctor_ids <- fread(doctor_list, header = FALSE)$V1

        covariates <- fread(covariate_file)
        covariates[, `:=`(
            SPECIALTY = as.character(INTERPRETATION),
            BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
            LICENSE_START = as.Date(START_DATE),
            LICENSE_END   = as.Date(END_DATE),
            INTERPRETATION = NULL
        )]

        events <- as.data.table(read_parquet(events_file))
        event_code_parts  <- strsplit(event_code, "_")[[1]]
        event_source      <- event_code_parts[1]
        event_actual_code <- event_code_parts[2]

        # Filter events based on the event code
        events <- events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
        event_ids <- unique(events$PATIENT_ID)

        # Load outcomes (N, Ni, and Y for desired medication)
        outcomes_cols <- c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes <- as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))

        # --- STEP 2: Data preparation (merge events, outcomes & covariates) ---
        events_new <- events[, .(PATIENT_ID, CODE, DATE)]
        events_new <- events_new[CODE == outcome_code]
        setnames(events_new, "PATIENT_ID", "DOCTOR_ID")
        # QC: Keep only the first event per DOCTOR_ID, in case multiple codes exist
        events_new <- events_new[order(DOCTOR_ID, DATE)]
        events_new <- events_new[, .SD[1], by = DOCTOR_ID]
        # QC: Ensure events are only for doctors in the doctor list
        outcomes_filtered <- outcomes[DOCTOR_ID %in% doctor_ids]

        df_merged <- events_new[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
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

        # --- STEP 3: Trim the medication's on-market window ---
        # (avoid bias from the drug entering/exiting the market during the study period)
        original_min_year <- min(df_complete[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
        original_max_year <- max(df_complete[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
        buffered_min_year <- original_min_year + BUFFER_YEARS
        buffered_max_year <- original_max_year - BUFFER_YEARS
        cat(sprintf("Original range of outcomes: %d-%d | Buffered range of outcomes: %d-%d\n", original_min_year, original_max_year, buffered_min_year, buffered_max_year))
        # Exclude events which happened before the first prescription of the outcome / or after the last one (using buffered range)
        df_complete <- df_complete[is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)]

        # --- STEP 4: Model data preparation ---
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

        # --- STEP 5: Define age tiers from fixed birth-year cutoffs ---
        # Create tier categories (for both cases and controls)
        df_model[, prescription_tier := fcase(
            BIRTH_YEAR < LOW_BEFORE_YEAR, paste0("Low (<", LOW_BEFORE_YEAR, ")"),
            BIRTH_YEAR > HIGH_AFTER_YEAR, paste0("High (>", HIGH_AFTER_YEAR, ")"),
            default = NA_character_
        )]

        # Convert prescription_tier to a factor with ordered levels
        df_model[, prescription_tier := factor(prescription_tier, levels = c(
            paste0("Low (<", LOW_BEFORE_YEAR, ")"),
            paste0("High (>", HIGH_AFTER_YEAR, ")")
        ))]

        # Report statistics on doctors per tier
        tier_stats <- df_model[, .(n_doctors = uniqueN(DOCTOR_ID)), by = prescription_tier][order(prescription_tier)]
        cat("\nDoctors per prescription tier:\n")
        print(tier_stats)

        # --- STEP 6: Stratified DiD - one model per age tier ---
        result_list_2 <- list()
        tiers <- levels(df_model$prescription_tier)[!is.na(levels(df_model$prescription_tier))]

        for (tier in tiers) {
            tryCatch({

            if (tier == tiers[1]) {
                df_tier <- df_model[BIRTH_YEAR < LOW_BEFORE_YEAR, ]
            } else {
                df_tier <- df_model[BIRTH_YEAR > HIGH_AFTER_YEAR, ]
            }

            n_cases_tier <- length(unique(df_tier[EVENT == 1, DOCTOR_ID]))
            n_controls_tier <- length(unique(df_tier[EVENT == 0, DOCTOR_ID]))

            # prepare variables as requested by did package
            df_tier$ID <- as.integer(factor(df_tier$DOCTOR_ID))
            df_tier$G <- ifelse(is.na(df_tier$EVENT_YEAR), 0, df_tier$EVENT_YEAR)
            df_tier$T <- df_tier$YEAR
            
            set.seed(09152024)
            att_gt_res_tier <- att_gt(
                yname = "Y",
                tname = "T",
                idname = "ID",
                gname = "G",
                xformla = ~ SEX + SPECIALTY,
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
            before_idx <- results$time %in% PRE_WINDOW
            after_idx <- results$time %in% POST_WINDOW

            # Meta-analysis of pre-period estimates
            pre_data <- data.frame(
                estimate = results$att[before_idx],
                se = results$se[before_idx]
            )
            pre_meta <- metafor::rma(yi = estimate, sei = se, data = pre_data, method = META_METHOD)
            avg_effect_before <- pre_meta$b[,1]
            se_pre <- pre_meta$se
            p_value_pre <- pre_meta$pval

            # Meta-analysis of post-period estimates
            post_data <- data.frame(
                estimate = results$att[after_idx],
                se = results$se[after_idx]
            )
            post_meta <- metafor::rma(yi = estimate, sei = se, data = post_data, method = META_METHOD)
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

            }, error = function(e) {
            if (tier == tiers[1]) {
                df_tier <- df_model[BIRTH_YEAR < LOW_BEFORE_YEAR, ]
            } else {
                df_tier <- df_model[BIRTH_YEAR > HIGH_AFTER_YEAR, ]
            }
            n_cases_tier        <- length(unique(df_tier[EVENT == 1, DOCTOR_ID]))
            n_controls_tier     <- length(unique(df_tier[EVENT == 0, DOCTOR_ID]))
            result_list_2[[tier]] <- data.frame(
                prescription_tier = tier,
                n_cases = n_cases_tier,
                n_controls = n_controls_tier,
                absolute_change = NA_real_,
                absolute_change_se = NA_real_,
                p_value_change = NA_real_
            )
            })
        }

        # combine results and save
        result_df <- do.call(rbind, result_list_2)
        result_df$code <- code
        result_list_1[[code]] <- result_df

    }, error = function(e) {
    cat(sprintf("Error processing code %s: %s\n", code, e$message))
    })
}


# ==============================================================================
# 5. COMBINE RESULTS, PIVOT TO WIDE FORMAT, TEST TIER DIFFERENCES
# ==============================================================================

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
        values_from = c(absolute_change, absolute_change_se, p_value_change, n_cases, n_controls, tier_range),
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
write.csv(results_wide, outfile, row.names = FALSE)