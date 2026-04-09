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
DATE = "20260316"
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE, '/Results_', DATE, '/Results_ATC_', DATE, '.csv')
events_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/processed_events_self_prescription_20260330.parquet"
outcomes_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedOutcomes_", DATE, "/processed_outcomes.parquet")
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
renamed_ATC_file = "/media/volume/Projects/ATC_renamed_codes.csv"
outdir = "/media/volume/Projects/DSGELabProject1/Plots/Results_20260316/"
if (!dir.exists(outdir)) {dir.create(outdir, recursive = TRUE)}

##### Main
dataset <- read_csv(dataset_file, show_col_types = FALSE)

# Filter only codes with at least 300 cases available
dataset <- dataset[dataset$N_CASES >= 300, ]

# Apply multiple test correction
dataset$PVAL_ADJ <- p.adjust(dataset$PVAL_ABS_CHANGE, method = "bonferroni")
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ < 0.05

# Apply correction also to the pre and post event p-values
dataset$PVAL_PRE_ADJ <- p.adjust(dataset$PVAL_PRE, method = "bonferroni")
dataset$PVAL_POST_ADJ <- p.adjust(dataset$PVAL_POST, method = "bonferroni")    

# Create a significance variable with two levels
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_CHANGE ~ "Significant",
  TRUE ~ "Not Significant"
)

# Extract list of significant medications for plots
code_list = dataset %>%
    filter(SIG_TYPE == "Significant") %>%
    pull(OUTCOME_CODE) %>%
    unique()

# -----------------------------------------------
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

        # Create a binary variable indicating self-prescription
        events[, SELF_PRESCRIPTION := ifelse(is.na(DOCTOR_ID), "Unknown", ifelse(PATIENT_ID==DOCTOR_ID, "Yes", "No"))]
        events[, DOCTOR_ID := NULL]

        # Load outcomes (N, Ni, and Y for desired medication)
        outcomes_cols = c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
        outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))

        # STEP 2: Data Preparation
        # Process and merge events and outcomes

        events_new = events[, .(PATIENT_ID, CODE, DATE, SELF_PRESCRIPTION)]
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

        # To ensure results are robust will apply "empirical bayes shrinkage" to doctors with low total prescriptions in a given year
        # Will shrink the ratio toward the mean within the doctor trajectory
        N_THRESHOLD = 5
        # Calculate mean Y for each doctor (using only observations where N >= N_THRESHOLD)
        df_model[, Y_mean := mean(Y[N >= N_THRESHOLD], na.rm = TRUE), by = DOCTOR_ID]
        # Apply empirical Bayes shrinkage: adjust Y values where N < N_THRESHOLD
        df_model[, Y := fifelse(
            N < N_THRESHOLD, 
            ((N * Y + N_THRESHOLD * Y_mean) / (N + N_THRESHOLD)), 
            Y
        )]
        df_model[, Y_mean := NULL]

        # Stratified analysis by tier
        result_list_2 <- list()
        tiers <- c("Yes", "No")

        for (tier in tiers) {
            df_tier <- df_model[SELF_PRESCRIPTION == tier | is.na(SELF_PRESCRIPTION),]
            
            n_cases_tier <- length(unique(df_tier[EVENT == 1, DOCTOR_ID]))
            n_controls_tier <- length(unique(df_tier[EVENT == 0, DOCTOR_ID]))

            # prepare variables as requested by did package
            df_tier$ID <- as.integer(factor(df_tier$DOCTOR_ID))                      
            df_tier$G <- ifelse(is.na(df_tier$EVENT_YEAR), 0, df_tier$EVENT_YEAR)    
            df_tier$T <- df_tier$YEAR    
            
            att_gt_res_tier <- att_gt(
                yname = "Y",
                tname = "T",
                idname = "ID",
                gname = "G",
                xformla = ~ SEX + BIRTH_YEAR + SPECIALTY,
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

            # Compute absolute change (post-event average ATT) and its SE
            results_post <- results[results$time >= 0, ]
            absolute_change <- mean(results_post$att, na.rm = TRUE)
            absolute_change_se <- sqrt(mean(results_post$se^2, na.rm = TRUE))
            p_value_change <- 2 * (1 - pnorm(abs(absolute_change / absolute_change_se)))

            result_list_2[[tier]] <- data.frame(
                code = code,
                tier_name = tier,
                absolute_change = absolute_change,
                absolute_change_se = absolute_change_se,
                p_value_change = p_value_change,
                n_cases = n_cases_tier,
                n_controls = n_controls_tier
            )
        }

        result_list_1[[code]] <- do.call(rbind, result_list_2)

    }, error = function(e) {
        cat(sprintf("Error for code %s: %s\n", code, e$message))
    })
}

# Combine results
combined_results <- do.call(rbind, result_list_1)

# Pivot to wide format
results_wide <- combined_results %>%
    pivot_wider(
        id_cols = code,
        names_from = tier_name,
        values_from = c(absolute_change, absolute_change_se, p_value_change, n_cases, n_controls),
        names_glue = "{tier_name}_{.value}"
    )

# Test if absolute changes are significantly different between tiers using z-test
results_wide$tier_significance <- apply(results_wide, 1, function(row) {
    # Extract estimates and SEs for Low and High tiers
    low_est <- as.numeric(row["Yes_absolute_change"])
    low_se <- as.numeric(row["Yes_absolute_change_se"])
    high_est <- as.numeric(row["No_absolute_change"])
    high_se <- as.numeric(row["No_absolute_change_se"])
    
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
write.csv(results_wide, paste0(outdir, "Supplements_StratifiedAnalysis_SelfPrescription_", DATE, ".csv"), row.names = FALSE)

# -----------------------------------------------
# Forest plot – stratified by self-prescription (Yes vs No)
# -----------------------------------------------

# Medications of interest: ATC code → readable label
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
        "ispaghula (psylla seeds)",
        "rosuvastatin",
        "etoricoxib",
        "frovatriptan",
        "zolpidem",
        "vortioxetine",
        "fluticasone furoate",
        "fluticasone, combinations",
        "vilanterol and fluticasone furoate"
    )
)

# reload data if running this section independently
results_wide <- read.csv(paste0(outdir, "Supplements_StratifiedAnalysis_SelfPrescription_", DATE, ".csv"))

# Pivot results_wide to long format: one row per (code x group)
plot_data <- bind_rows(
    results_wide %>% transmute(code, group = "Yes", absolute_change = Yes_absolute_change, absolute_change_se = Yes_absolute_change_se, n_cases = Yes_n_cases),
    results_wide %>% transmute(code, group = "No",  absolute_change = No_absolute_change,  absolute_change_se = No_absolute_change_se,  n_cases = No_n_cases)
) %>%
    left_join(code_labels, by = c("code" = "OUTCOME_CODE")) %>%
    mutate(
        ci_lo = absolute_change - 1.96 * absolute_change_se,
        ci_hi = absolute_change + 1.96 * absolute_change_se
    )

# Order medications by mean effect
label_order <- dataset %>%
    filter(OUTCOME_CODE %in% plot_data$code) %>%
    arrange(ABS_CHANGE) %>%
    pull(OUTCOME_CODE) %>%
    unique() %>%
    {tibble(code = .) %>% left_join(code_labels, by = c("code" = "OUTCOME_CODE")) %>% pull(LABEL)}

# Rescale n_cases to dot size
n_range <- range(plot_data$n_cases, na.rm = TRUE)
rescale_size <- function(x, from = n_range, to = c(1.5, 8)) {
    if (diff(from) == 0) return(rep(mean(to), length(x)))
    (x - from[1]) / diff(from) * diff(to) + to[1]
}

DODGE <- 0.22   # vertical separation between Yes/No within each medication row

plot_data <- plot_data %>%
    mutate(
        LABEL   = factor(LABEL, levels = label_order),
        group   = factor(group, levels = c("Yes", "No")),
        y_pos   = as.numeric(LABEL) + ifelse(group == "Yes", +DODGE, -DODGE),
        pt_size = rescale_size(n_cases)
    )

forest_plot <- ggplot(plot_data, aes(x = absolute_change, y = y_pos, colour = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.5) +
    geom_errorbarh(
        aes(xmin = ci_lo, xmax = ci_hi),
        height = DODGE * 0.7, linewidth = 0.65, na.rm = TRUE
    ) +
    geom_point(aes(size = pt_size), shape = 16, na.rm = TRUE) +
    scale_y_continuous(
        breaks = seq_along(label_order),
        labels = label_order,
        expand = expansion(add = 0.6)
    ) +
    scale_size_identity() +
    scale_colour_manual(
        values = c("Yes" = "#E41A1C", "No" = "#377EB8"),
        name   = "Self-prescription"
    ) +
    labs(
        x     = "Absolute change in prescription rate (95% CI)",
        y     = NULL,
        title = "Stratified analysis by self-prescription"
    ) +
    annotate("text", x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.5,
             label = sprintf("Dot size proportional to n cases"),
             size = 2.5, colour = "grey50") +
    theme_bw(base_size = 9) +
    theme(
        axis.text.y        = element_text(size = 10, face = "bold"),
        axis.text.x        = element_text(size = 10),
        panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.3),
        panel.grid.minor   = element_blank(),
        legend.position    = "right",
        legend.title       = element_text(face = "bold", size = 10),
        legend.text        = element_text(size = 10),
        plot.title         = element_text(face = "bold", size = 10),
        plot.margin        = margin(8, 12, 8, 8)
    ) +
    guides(colour = guide_legend(override.aes = list(size = 4)))

ggsave(
    filename = paste0(outdir, "ForestPlot_StratifiedAnalysis_SelfPrescription_", DATE, ".png"),
    plot     = forest_plot,
    width    = 10,
    height   = 10,
    units    = "in",
    dpi      = 300
)