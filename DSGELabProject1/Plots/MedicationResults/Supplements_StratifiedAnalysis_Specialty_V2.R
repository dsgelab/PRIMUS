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
events_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedEvents_", DATE, "/processed_events.parquet")
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
# Minimum number of cases and controls required per specialty
N_MIN = 5

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

        # Replace empty string with "No Specialty"
        df_model[SPECIALTY == "", SPECIALTY := "No Specialty"]

        result_list_2 <- list()
        all_specialties <- unique(as.character(df_model$SPECIALTY))

        for (specialty in all_specialties) {
            tryCatch({
                df_spec <- df_model[SPECIALTY == specialty, ]
                n_cases_spec    <- length(unique(df_spec[EVENT == 1, DOCTOR_ID]))
                n_controls_spec <- length(unique(df_spec[EVENT == 0, DOCTOR_ID]))

                # Skip specialty if either cases or controls do not exceed threshold
                if (n_cases_spec <= N_MIN | n_controls_spec <= N_MIN) {
                    cat(sprintf("  Skipping specialty '%s' for code %s: cases=%d, controls=%d\n", specialty, code, n_cases_spec, n_controls_spec))
                    next
                }

                # prepare variables as requested by did package
                df_spec$ID <- as.integer(factor(df_spec$DOCTOR_ID))
                df_spec$G  <- ifelse(is.na(df_spec$EVENT_YEAR), 0, df_spec$EVENT_YEAR)
                df_spec$T  <- df_spec$YEAR

                att_gt_res_spec <- att_gt(
                    yname = "Y",
                    tname = "T",
                    idname = "ID",
                    gname = "G",
                    xformla = ~ BIRTH_YEAR + SEX,
                    data = df_spec,
                    est_method = "dr",
                    control_group = "notyettreated",
                    clustervars = "ID",
                    pl = TRUE,
                    cores = N_THREADS
                )

                agg_dynamic <- aggte(att_gt_res_spec, type = "dynamic", na.rm = TRUE)
                results <- data.frame(
                    time = agg_dynamic$egt,
                    att  = agg_dynamic$att.egt,
                    se   = agg_dynamic$se.egt
                )

                # For medications results will consider ATT and SE in a 3 year window before and after event (t=0)
                before_idx <- results$time %in% c(-3, -2, -1)
                after_idx  <- results$time %in% c(1, 2, 3)

                # Meta-analysis of pre-period estimates
                pre_data <- data.frame(
                    estimate = results$att[before_idx],
                    se       = results$se[before_idx]
                )
                pre_meta        <- metafor::rma(yi = estimate, sei = se, data = pre_data, method = "FE")
                avg_effect_before <- pre_meta$b[, 1]
                se_pre          <- pre_meta$se
                p_value_pre     <- pre_meta$pval

                # Meta-analysis of post-period estimates
                post_data <- data.frame(
                    estimate = results$att[after_idx],
                    se       = results$se[after_idx]
                )
                post_meta        <- metafor::rma(yi = estimate, sei = se, data = post_data, method = "FE")
                avg_effect_after <- post_meta$b[, 1]
                se_post          <- post_meta$se
                p_value_post     <- post_meta$pval

                # Absolute change and relative change estimates
                absolute_change    <- avg_effect_after - avg_effect_before
                absolute_change_se <- sqrt(se_post^2 + se_pre^2)
                score_abs          <- absolute_change / absolute_change_se
                p_value_change     <- 2 * (1 - pnorm(abs(score_abs)))

                result_list_2[[specialty]] <- data.frame(
                    code               = code,
                    specialty          = specialty,
                    absolute_change    = round(absolute_change, 5),
                    absolute_change_se = round(absolute_change_se, 5),
                    p_value            = round(p_value_change, 5),
                    n_cases            = n_cases_spec,
                    n_controls         = n_controls_spec
                )

            }, error = function(e) {
                df_spec         <- df_model[SPECIALTY == specialty, ]
                n_cases_spec    <- length(unique(df_spec[EVENT == 1, DOCTOR_ID]))
                n_controls_spec <- length(unique(df_spec[EVENT == 0, DOCTOR_ID]))
                cat(sprintf("  Error for specialty '%s', code %s: %s\n", specialty, code, e$message))
                result_list_2[[specialty]] <<- data.frame(
                    code               = code,
                    specialty          = specialty,
                    absolute_change    = NA_real_,
                    absolute_change_se = NA_real_,
                    p_value            = NA_real_,
                    n_cases            = n_cases_spec,
                    n_controls         = n_controls_spec
                )
            })
        }

        # Combine specialty results for this code and append to main list
        if (length(result_list_2) > 0) {
            result_df <- do.call(rbind, result_list_2)
            result_list_1[[code]] <- result_df
        }

    }, error = function(e) {
        cat(sprintf("Error processing code %s: %s\n", code, e$message))
    })
}

# Combine all results into one long file with the required header
combined_results <- do.call(rbind, result_list_1)
rownames(combined_results) <- NULL

# Save final results
write.csv(combined_results,
          paste0(outdir, "Supplements_StratifiedAnalysis_Specialty_V2_", DATE, ".csv"),
          row.names = FALSE)


# -----------------------------------------------
# FOREST PLOT: absolute change by specialty
# -----------------------------------------------

# uncomment to read in file, if running separately from the above code block 
# combined_results <- read_csv(paste0(outdir, "Supplements_StratifiedAnalysis_Specialty_V2_", DATE, ".csv"), show_col_types = FALSE)

# 1. Build complete grid and compute CIs (same as before)
all_specialties_global <- sort(unique(combined_results$specialty))
all_codes              <- sort(unique(combined_results$code))

plot_grid <- expand.grid(
    code      = all_codes,
    specialty = all_specialties_global,
    stringsAsFactors = FALSE
)

plot_data <- plot_grid %>%
    left_join(combined_results, by = c("code", "specialty")) %>%
    mutate(
        absolute_change = ifelse(
            !is.na(n_cases) & !is.na(n_controls) & n_cases > 30 & n_controls > 30,
            absolute_change, NA_real_
        ),
        absolute_change_se = ifelse(
            !is.na(n_cases) & !is.na(n_controls) & n_cases > 30 & n_controls > 30,
            absolute_change_se, NA_real_
        ),
        ci_lo = absolute_change - 1.96 * absolute_change_se,
        ci_hi = absolute_change + 1.96 * absolute_change_se
    ) %>%
    group_by(specialty) %>%
    filter(any(!is.na(absolute_change))) %>%   # drop specialties with no available estimate
    ungroup()

# Keep only specialties that still have at least one estimate
all_specialties_global <- all_specialties_global[all_specialties_global %in% unique(plot_data$specialty)]

plot_data <- plot_data %>%
    mutate(
        specialty = factor(specialty, levels = rev(all_specialties_global)),
        code = factor(code, levels = all_codes)
    )

# 2. Dodge offset so multiple codes per specialty row don't overlap
#    position_dodgev() requires ggstance; use a manual numeric offset instead
n_codes     <- length(all_codes)
dodge_step  <- 0.7 / n_codes                          # total band = 0.7 units
code_offsets <- seq(-(n_codes - 1) / 2, (n_codes - 1) / 2) * dodge_step
names(code_offsets) <- all_codes

plot_data <- plot_data %>%
    mutate(y_dodge = as.numeric(specialty) + code_offsets[as.character(code)])

# 3. Colour palette — up to 9 codes; uses a colorblind-friendly qualitative ramp
code_colours <- setNames(
    RColorBrewer::brewer.pal(n_codes, "Set1"),
    all_codes
)

# 4. Draw the plot
plot_data <- plot_data %>%
    mutate(alpha_val = ifelse(!is.na(p_value) & p_value <= 0.05, 1, 0.1))

forest_plot <- ggplot(plot_data, aes(x = absolute_change, y = y_dodge, colour = code)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.5) +
    geom_errorbarh(
        aes(xmin = ci_lo, xmax = ci_hi, alpha = alpha_val),
        height    = dodge_step * 0.6,
        linewidth = 0.5,
        na.rm     = TRUE
    ) +
    geom_point(
        aes(alpha = alpha_val),
        shape = 18,
        size  = 2.2,
        na.rm = TRUE
    ) +
    # Restore specialty labels on Y axis
    scale_y_continuous(
        breaks = seq_along(all_specialties_global),
        labels = rev(all_specialties_global)
    ) +
    scale_colour_manual(values = code_colours, name = "Code") +
    scale_alpha_identity() +
    labs(
        x       = "Absolute change in prescription rate (95 % CI)",
        y       = NULL,
        title   = "Stratified analysis by specialty"
    ) +
    theme_bw(base_size = 9) +
    theme(
        axis.text.y        = element_text(size = 7),
        axis.text.x        = element_text(size = 7),
        panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.3),
        panel.grid.minor   = element_blank(),
        legend.position    = "right",
        legend.title       = element_text(face = "bold", size = 8),
        legend.text        = element_text(size = 7),
        plot.title         = element_text(face = "bold", size = 11),
        plot.caption       = element_text(size = 7, colour = "grey50"),
        plot.margin        = margin(8, 12, 8, 8)
    )

# 5. Save
ggsave(
    filename = paste0(outdir, "ForestPlot_StratifiedAnalysis_Specialty_AllCodes_30MIN_", DATE, ".png"),
    plot     = forest_plot,
    width    = 12,
    height   = 8,
    units    = "in",
    dpi      = 300
)

# -----------------------------------------------
# Table with Q statistic for heterogeneity across specialties, for each medication code
# -----------------------------------------------

# Keep only specialties with at least 30 cases AND 30 controls
N_MIN_PLOT <- 30
 
combined_results_filtered <- combined_results %>%
    filter(
        !is.na(n_cases)          & !is.na(n_controls) &
        n_cases    >= N_MIN_PLOT & n_controls >= N_MIN_PLOT &
        !is.na(absolute_change)  & !is.na(absolute_change_se) 
    )
write.csv(combined_results_filtered,paste0(outdir, "Supplements_StratifiedAnalysis_Specialty_V2_MIN30_", DATE, ".csv"),row.names = FALSE)

 
# Cochran's Q test for heterogeneity across specialties, per medication code
heterogeneity_list <- list()
 
for (med_code in unique(combined_results_filtered$code)) {
 
    df_code <- combined_results_filtered %>% filter(code == med_code)
 
    if (nrow(df_code) < 2) {
        # Need at least 2 specialties to test heterogeneity
        heterogeneity_list[[med_code]] <- data.frame(
            code          = med_code,
            n_specialties = nrow(df_code),
            Q_stat        = NA_real_,
            Q_df          = NA_integer_,
            Q_pval        = NA_real_,
            I2            = NA_real_,
            stringsAsFactors = FALSE
        )
        cat(sprintf("  Code %s: only %d specialty after filtering – Q test skipped.\n", med_code, nrow(df_code)))
        next
    }
 
    tryCatch({
        meta_res <- metafor::rma(
            yi     = absolute_change,
            sei    = absolute_change_se,
            data   = df_code,
            method = "FE"          # fixed-effect model gives the standard Cochran Q
        )
        heterogeneity_list[[med_code]] <- data.frame(
            code          = med_code,
            n_specialties = meta_res$k,
            Q_stat        = meta_res$QE,
            Q_df          = as.integer(meta_res$k - 1),
            Q_pval        = meta_res$QEp,
            I2            = meta_res$I2,
            stringsAsFactors = FALSE
        )
    }, error = function(e) {
        cat(sprintf("  Q-test error for code %s: %s\n", med_code, e$message))
        heterogeneity_list[[med_code]] <<- data.frame(
            code          = med_code,
            n_specialties = nrow(df_code),
            Q_stat        = NA_real_,
            Q_df          = NA_integer_,
            Q_pval        = NA_real_,
            I2            = NA_real_,
            stringsAsFactors = FALSE
        )
    })
}
 
# Stand-alone heterogeneity table (one row per medication code)
heterogeneity_table <- do.call(rbind, heterogeneity_list)
rownames(heterogeneity_table) <- NULL
 
# Bonferroni-adjust Q p-values across all tested codes
heterogeneity_table$Q_pval_adj   <- p.adjust(heterogeneity_table$Q_pval, method = "bonferroni")
heterogeneity_table$heterogeneous <- ifelse(
    !is.na(heterogeneity_table$Q_pval_adj),
    heterogeneity_table$Q_pval_adj < 0.05,
    NA
)
write.csv(heterogeneity_table,paste0(outdir, "Supplements_HeterogeneityQ_ByCode_", DATE, ".csv"),row.names = FALSE)
 

 