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
events_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version3_Highthroughput/ProcessedEvents_20251020/processed_events.parquet"
outcomes_file1 = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version3_Highthroughput/ProcessedOutcomes_20251020/processed_outcomes.parquet"
outcomes_file2 = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput_drop/ProcessedOutcomes_20251028/processed_outcomes.parquet"
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
outdir = "/media/volume/Projects/DSGELabProject1/DiD_Validation/"

# Test 1
event_code = 'Purch_C10AA07'
outcome_code = 'C10AA07'
today <- format(Sys.Date(), "%Y%m%d")
outdir = file.path(outdir, paste0("ValidationPlots_", outcome_code, "_", today))
if (!dir.exists(outdir)) {dir.create(outdir, recursive = TRUE)}

#### Main
N_THREADS = 10
setDTthreads(N_THREADS) 
# not using all threads to easily run in background

# STEP 1: Data Loading 
covariates = fread(covariate_file)
doctor_ids = fread(doctor_list, header = FALSE)$V1

events = as.data.table(read_parquet(events_file))
event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1]
event_actual_code = event_code_parts[2]

# Filter events based on the event code
events = events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
event_ids = unique(events$PATIENT_ID)

# Load outcomes (N, Ni, and Y for desired medication)
outcomes_cols1 = c("DOCTOR_ID", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
outcomes1 = as.data.table(read_parquet(outcomes_file1, col_select = outcomes_cols1))
outcomes_cols2 = c("DOCTOR_ID", "YEAR","N")
outcomes2 = as.data.table(read_parquet(outcomes_file2, col_select = outcomes_cols2))
outcomes = merge(
    outcomes1,
    outcomes2,
    by = c("DOCTOR_ID", "YEAR"),
    all.x = TRUE,
    suffixes = c("", "_drop")
)

# STEP 2: Data Preparation
# Process and merge events and outcomes

events = events[, .(PATIENT_ID, CODE, DATE)]
setnames(events, "PATIENT_ID", "DOCTOR_ID")
# QC: Keep only the first event per DOCTOR_ID, in case multiple codes exist
events = events[order(DOCTOR_ID, DATE)]
events = events[, .SD[1], by = DOCTOR_ID]
# QC: Ensure events are only for doctors in the doctor list
outcomes_filtered = outcomes[DOCTOR_ID %in% doctor_ids]

df_merged = events[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
df_merged[, DATE := as.Date(DATE)]
df_merged[, EVENT := ifelse(!is.na(DATE), 1, 0)]
df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
df_merged[, DATE := NULL]

# Process prescription timeframe
# 1. Calculate original min and max year across all doctors in the cohort
original_min_year <- min(df_merged[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
original_max_year <- max(df_merged[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
# 2. Add buffer to min and max year to avoid bias due to medications entering or exiting the market
buffer_years = 1
buffered_min_year <- original_min_year + buffer_years
buffered_max_year <- original_max_year - buffer_years
cat(sprintf("Original range of outcomes: %d-%d | Buffered range of outcomes: %d-%d\n",
    original_min_year,
    original_max_year,
    buffered_min_year,
    buffered_max_year
))
# Remove all information outside of buffered range
df_merged <- df_merged[YEAR >= buffered_min_year & YEAR <= buffered_max_year]
# 3. Exclude events which happened before the first prescription of the outcome / or after the last one (using buffered range)
df_merged <- df_merged[is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)]

# Prepare covariates 
covariates[, `:=`(
    SPECIALTY = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))
)]
covariates[, `:=`(
    BIRTH_DATE = NULL, 
    INTERPRETATION = NULL)
]

# Merge covariates
df_complete = covariates[df_merged, on = "DOCTOR_ID"]
df_complete[, `:=`(
    AGE = YEAR - BIRTH_YEAR,
    AGE_IN_2023 = 2023 - BIRTH_YEAR,
    AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
)]

# Filter out events after 60 and prescriptions after 60
events_after60 = df_complete[AGE_AT_EVENT > 60 & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
df_complete = df_complete[!(DOCTOR_ID %in% events_after60) & AGE <= 60]

# STEP 3: Analysis using 'did' package
df_model <- as.data.table(df_complete)[
    , `:=`(
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
        Y = get(paste0("Y_", outcome_code)),
        Ni = get(paste0("N_", outcome_code))
    )
]

# Analysis requires only individuals with non-missing Y for all years in the required window
years_required <- (min(df_model$YEAR, na.rm = TRUE)):(max(df_model$YEAR, na.rm = TRUE))
ids_with_all_years <- df_model %>%
    filter(YEAR %in% years_required & !is.na(Y)) %>%
    group_by(DOCTOR_ID) %>%
    summarise(n_years = n_distinct(YEAR)) %>%
    filter(n_years == length(years_required)) %>%
    pull(DOCTOR_ID)

#prepare the model data
df_model <- df_model %>% filter(DOCTOR_ID %in% ids_with_all_years, YEAR %in% years_required)
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                       # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)  # G = group of first treatment year, 0 for never-treated
df_model$T <- df_model$YEAR    

# Calculate number of cases (events) and controls
n_cases <- length(unique(df_model[df_model$EVENT == 1, DOCTOR_ID]))
n_controls <- length(unique(df_model[df_model$EVENT == 0, DOCTOR_ID]))

# ---------------   
# VALIDATION ANALYSIS: Run DiD model separately for four different age groups
# ---------------

AGE_BINS = c(-Inf, 30, 40, 50, 60)

# Analysis 1: Using Y (ratio outcome) - Split by AGE_AT_EVENT bins
results_by_age <- list()
plots_by_age <- list()

for (i in 1:(length(AGE_BINS) - 1)) {
    age_min <- AGE_BINS[i]
    age_max <- AGE_BINS[i + 1]
    
    # Filter data for this age bin
    # For cases (EVENT == 1): filter by AGE_AT_EVENT
    # For controls (EVENT == 0): no filter 
    df_age <- df_model %>% filter((EVENT == 1 & AGE_AT_EVENT >= age_min & AGE_AT_EVENT < age_max) | (EVENT == 0))
    
    # Get n of cases and controls doctor for this age group
    n_cases_age <- length(unique(df_age[df_age$EVENT == 1, ]$DOCTOR_ID))
    n_controls_age <- length(unique(df_age[df_age$EVENT == 0, ]$DOCTOR_ID))
    
    # Run DiD model for this age group
    att_gt_res_Y_age <- att_gt(
        yname = "Y",
        tname = "T",
        idname = "ID",
        gname = "G",
        xformla = ~ BIRTH_YEAR + SEX + SPECIALTY,
        data = df_age,
        est_method = "dr",
        control_group = "notyettreated",
        clustervars = "ID",
        pl = TRUE,
        cores = N_THREADS
    )
    
    agg_dynamic_Y_age <- aggte(att_gt_res_Y_age, type = "dynamic", na.rm = TRUE)
    results_Y_age <- data.frame(
        time = agg_dynamic_Y_age$egt,
        att = agg_dynamic_Y_age$att.egt,
        se = agg_dynamic_Y_age$se.egt,
        age_group = sprintf("Age %s-%s", as.character(age_min), as.character(age_max)),
        n_cases = n_cases_age,
        n_controls = n_controls_age
    )
    
    results_by_age[[i]] <- results_Y_age
}

# Combine all age group results
results_combined_age <- do.call(rbind, results_by_age)
data_plot_age <- results_combined_age[results_combined_age$time >= -3 & results_combined_age$time <= 3, ]
data_plot_age$age_group_label <- paste0(
    data_plot_age$age_group,
    "\n(Cases: ", data_plot_age$n_cases,
    ", Controls: ", data_plot_age$n_controls, ")"
)

# Create plots for each age group, replacing with empty plot if < 5 cases
plots_by_age <- list()
for (i in 1:(length(AGE_BINS) - 1)) {
    age_min <- AGE_BINS[i]
    age_max <- AGE_BINS[i + 1]
    age_label <- sprintf("Age %s-%s", as.character(age_min), as.character(age_max))
    
    # Get data for this age group
    data_age <- data_plot_age[data_plot_age$age_group == age_label, ]
    n_cases_age <- unique(data_age$n_cases)[1]
    n_controls_age <- unique(data_age$n_controls)[1]
    
    if (n_cases_age < 5) {
        # Create empty plot with text
        plots_by_age[[i]] <- ggplot() +
            annotate("text", x = 0, y = 0, label = sprintf("Insufficient data \n(Cases: %d)", n_cases_age, n_controls_age),size = 5, color = "gray50") +
            theme_void() +
            theme(plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.5))
    } else {
        # Create normal plot
        plots_by_age[[i]] <- ggplot(data_age, aes(x = time, y = att)) +
            geom_line(color = "#1f77b4") +
            geom_point() +
            geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            labs(
                subtitle = sprintf("%s\n(Cases: %d, Controls: %d)", age_label, n_cases_age, n_controls_age),
                x = "Years from Event",
                y = "Change in Prescription Behavior\n(Difference in Difference ATT)"
            ) +
            theme_minimal() +
            theme(
                plot.subtitle = element_text(face = "bold", size = 10),
                axis.title.x = element_text(size = 9),
                axis.title.y = element_text(size = 9)
            )
    }
}

# Combine all plots vertically
p_age_combined <- wrap_plots(plots_by_age, ncol = 1) +
    plot_annotation(
        title = paste0("Effect of ", event_code, " on ", outcome_code, " by Age at Event"),
        theme = theme(plot.title = element_text(size = 12, face = "bold"))
    )
ggsave(file.path(outdir, paste0("Validation2.png")), p_age_combined, width = 8, height = 12)

# Extract metric for general validation
# Extract post-event coefficients (time 0, 1, 2) and pre-event coefficients (time -1, -2, -3)
# For Y (Prescription ratio) and by age group

results_Y_post_age <- list()
results_Y_pre_age <- list()
Y_delta_age <- numeric(length(AGE_BINS) - 1)
Y_delta_SE_age <- numeric(length(AGE_BINS) - 1)
Y_zscore_age <- numeric(length(AGE_BINS) - 1)

for (i in 1:(length(AGE_BINS) - 1)) {
    age_min <- AGE_BINS[i]
    age_max <- AGE_BINS[i + 1]
    # Filter results for this age group
    results_Y_age <- results_by_age[[i]]
    results_Y_post_age[[i]] <- results_Y_age[results_Y_age$time %in% c(0, 1, 2), ]
    results_Y_pre_age[[i]] <- results_Y_age[results_Y_age$time %in% c(-1, -2, -3), ]
    # calculaet Z-score
    Y_delta_age[i] <- mean(results_Y_post_age[[i]]$att, na.rm = TRUE) - mean(results_Y_pre_age[[i]]$att, na.rm = TRUE)
    Y_delta_SE_age[i] <- sqrt(sum(results_Y_post_age[[i]]$se^2, na.rm = TRUE)/nrow(results_Y_post_age[[i]]) + sum(results_Y_pre_age[[i]]$se^2, na.rm = TRUE)/nrow(results_Y_pre_age[[i]]))
    Y_zscore_age[i] <- Y_delta_age[i] / Y_delta_SE_age[i]
}

# Create data frame with results by age group
age_group_labels <- sprintf("Age %s-%s", as.character(AGE_BINS[-length(AGE_BINS)]), as.character(AGE_BINS[-1]))
validation_by_age <- data.frame(
    age_group = age_group_labels,
    Y_delta = Y_delta_age,
    Y_delta_SE = Y_delta_SE_age,
    Y_zscore = Y_zscore_age
)

# calculate average 

# test scatter plot of Ni vs N z-scores
delta_df <- data.frame(
    labels = c("N", "Ni"),
    zscores = c(N_zscore, Ni_zscore)
)   
p2 <- ggplot(delta_df, aes(x = zscores[1], y = zscores[2])) +
    geom_point(size = 3, color = "#1f77b4") +
    labs(
        title = "Scatterplot of Z-scores for Delta ATT Post vs Pre Event",
        x = "Z-score N",
        y = "Z-score Ni"
    ) +
    theme_minimal() +
    xlim(-max(abs(delta_df$zscores)), max(abs(delta_df$zscores))) +
    ylim(-max(abs(delta_df$zscores)), max(abs(delta_df$zscores))) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red")
