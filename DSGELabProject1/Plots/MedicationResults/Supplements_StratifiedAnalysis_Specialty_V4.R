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
    library(scales)
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
N_MIN = 10

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

                # extract baseline prescription rate among controls (EVENT == 0) for relative change calculation
                baseline <- df_spec[df_spec$EVENT == 0, ] %>% summarise(baseline = mean(Y, na.rm = TRUE)) %>% pull(baseline)

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
                relative_change    <- ifelse(baseline != 0, (absolute_change + baseline) / baseline, NA_real_)

                result_list_2[[specialty]] <- data.frame(
                    code               = code,
                    specialty          = specialty,
                    baseline           = round(baseline, 5),
                    absolute_change    = round(absolute_change, 5),
                    absolute_change_se = round(absolute_change_se, 5),
                    relative_change    = round(relative_change, 5),
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
                    baseline           = baseline, 
                    absolute_change    = NA_real_,
                    absolute_change_se = NA_real_,
                    relative_change    = NA_real_,
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
          paste0(outdir, "Supplements_StratifiedAnalysis_Specialty_V4_", DATE, ".csv"),
          row.names = FALSE)


# -----------------------------------------------
# -----------------------------------------------
# STRATIFIED GRID PLOT: 2x5 grid, one panel per medication
# Each panel: A) baseline bar chart | B) relative change CI
# Shared y-axis (ALL global specialties, always) on left panel only
# Medication name as plot title top-left of each panel
# -----------------------------------------------

# reload data if running separately from the above code block
combined_results <- read_csv(paste0(outdir, "Supplements_StratifiedAnalysis_Specialty_V4_", DATE, ".csv"), show_col_types = FALSE)

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

# Apply multiple-testing correction within stratified results
combined_results <- combined_results %>%
    mutate(
        p_value_adj = p.adjust(p_value, method = "fdr"),
        significant = !is.na(p_value_adj) & p_value_adj < 0.05
    )

# Join labels
combined_results <- combined_results %>%
    left_join(code_labels, by = c("code" = "OUTCOME_CODE")) %>%
    mutate(med_label = ifelse(!is.na(LABEL), LABEL, code), LABEL = NULL)

# Mask estimates below sample threshold and compute CIs
combined_results <- combined_results %>%
    mutate(
        across(
            c(absolute_change, absolute_change_se, relative_change, baseline),
            ~ ifelse(is.na(n_cases) | is.na(n_controls) | n_cases < N_MIN | n_controls < N_MIN, NA_real_, .)
        ),
        # CI for relative change (propagated from absolute SE / baseline)
        relative_change_se = abs(absolute_change_se / baseline),
        relative_change_lo = relative_change - 1.96 * relative_change_se,
        relative_change_hi = relative_change + 1.96 * relative_change_se,
        significant = ifelse(is.na(relative_change), FALSE, significant)
    )

# Global specialty ordering: alphabetical, reversed so top of plot = first alphabetically
all_specialties_global <- sort(unique(combined_results$specialty))
sp_levels_global       <- rev(all_specialties_global)  # reversed for ggplot y-axis

# Shared theme elements
base_theme <- theme_minimal(base_size = 8) +
    theme(
        panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.25),
        panel.grid.major.x = element_line(colour = "grey93", linewidth = 0.25),
        panel.grid.minor   = element_blank(),
        axis.ticks         = element_blank(),
        plot.margin        = margin(2, 4, 2, 4)
    )

# Dark green accent colour
PANEL_COLOUR <- "#1B5E20"

# Cap height for CI error bars (fraction of one y unit)
cap_height <- 0.3
pt_size    <- 1.8

# --- Build one pair of plots (A | B) per medication ---
make_med_plots <- function(med_code, med_label, df_all, sp_levels) {

    # Filter to this medication; keep ALL global specialties via full join
    df_med <- df_all %>% filter(code == med_code)

    # Build a complete skeleton with every global specialty
    skeleton <- tibble(specialty = sp_levels)  # all specialties, reversed order

    df <- skeleton %>%
        left_join(df_med, by = "specialty") %>%
        mutate(
            specialty_f = factor(specialty, levels = sp_levels)
        )

    # create y-axis labels that include counts per specialty
    df <- df %>%
        mutate(
            n_cases_disp = ifelse(is.na(n_cases), 0L, as.integer(n_cases)),
            n_controls_disp = ifelse(is.na(n_controls), 0L, as.integer(n_controls)),
            y_label = paste0(specialty, "\n(n cases = ", n_cases_disp, "  n controls = ", n_controls_disp, ")")
        )

    # ---- Panel A: Horizontal bar plot of baseline ----
    avg_baseline <- mean(df$baseline, na.rm = TRUE)
    df_a <- df  # include all rows; missing baseline -> bar simply absent

    panel_a <- ggplot(df_a, aes(x = baseline, y = specialty_f)) +
        geom_col(fill = PANEL_COLOUR, colour = NA, width = 0.65, na.rm = TRUE) +
        geom_vline(
            xintercept = avg_baseline,
            linetype   = "dashed",
            colour     = "grey30",
            linewidth  = 0.5,
            na.rm      = TRUE
        ) +
        scale_y_discrete(
            limits = sp_levels,
            labels = df_a$y_label,
            drop   = FALSE
        ) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
        labs(x = "Baseline rate \n(in controls)", y = NULL, title = med_label) +
        annotate("text",
                 x     = avg_baseline,
                 y     = Inf,
                 label = "mean",
                 hjust = -0.15, vjust = 1.4,
                 size  = 2.0, colour = "grey30") +
        base_theme +
        theme(
            axis.text.y  = element_text(size = 10),
            axis.text.x  = element_text(size = 10),
            plot.title   = element_text(
                size   = 14,
                face   = "bold",
                color  = "black",
                hjust  = 0,
                margin = margin(b = 3)
            )
        )

    # ---- Panel B: Relative change with 95% CI ----
    # All specialties present on y-axis; those without data simply have no geom drawn
    df_b <- df  # full skeleton; missing rows will be silently skipped by na.rm

    panel_b <- ggplot(df_b) +
        geom_vline(xintercept = 1, linetype = "dashed",colour = "grey60", linewidth = 0.4) +
        # CI whisker
        geom_segment(
            data = df_b %>% filter(!is.na(relative_change)),
            aes(
                x     = relative_change_lo, xend = relative_change_hi,
                y     = specialty_f,        yend = specialty_f,
                alpha = ifelse(significant, 1.0, 0.12)
            ),
            colour    = PANEL_COLOUR,
            linewidth = 0.45
        ) +
        # CI caps lower
        geom_segment(
            data = df_b %>% filter(!is.na(relative_change)),
            aes(
                x     = relative_change_lo, xend = relative_change_lo,
                y     = as.numeric(specialty_f) - cap_height,
                yend  = as.numeric(specialty_f) + cap_height,
                alpha = ifelse(significant, 1.0, 0.12)
            ),
            colour    = PANEL_COLOUR,
            linewidth = 0.35
        ) +
        # CI caps upper
        geom_segment(
            data = df_b %>% filter(!is.na(relative_change)),
            aes(
                x     = relative_change_hi, xend = relative_change_hi,
                y     = as.numeric(specialty_f) - cap_height,
                yend  = as.numeric(specialty_f) + cap_height,
                alpha = ifelse(significant, 1.0, 0.12)
            ),
            colour    = PANEL_COLOUR,
            linewidth = 0.35
        ) +
        # Point estimate
        geom_point(
            data = df_b %>% filter(!is.na(relative_change)),
            aes(
                x     = relative_change,
                y     = specialty_f,
                alpha = ifelse(significant, 1.0, 0.12)
            ),
            colour = PANEL_COLOUR,
            shape  = 16,
            size   = pt_size
        ) +
        scale_x_continuous(
            limits = c(-6, 6),
            breaks = seq(-6, 6, by = 2)
        ) +
        scale_y_discrete(
            limits = sp_levels,
            drop   = FALSE
        ) +
        scale_alpha_identity() +
        labs(x = "Relative change (95% CI)", y = NULL) +
        base_theme +
        theme(
            axis.text.y  = element_blank(),
            axis.text.x  = element_text(size = 10, colour = "grey30")
        )

    # ---- Combine A | B; y-axis labels only on A ----
    triplet <- (panel_a | panel_b) +
        plot_layout(widths = c(1.6, 1))

    return(triplet)
}

# --- Generate one pair per medication ---
med_codes  <- code_labels$OUTCOME_CODE[code_labels$OUTCOME_CODE %in% unique(combined_results$code)]
med_labels <- code_labels$LABEL[code_labels$OUTCOME_CODE %in% med_codes]

plot_list <- mapply(
    FUN       = make_med_plots,
    med_code  = med_codes,
    med_label = med_labels,
    MoreArgs  = list(df_all = combined_results, sp_levels = sp_levels_global),
    SIMPLIFY  = FALSE
)

# --- Summary barplot: number of medications with significant change per specialty ---
sig_counts <- combined_results %>%
    filter(significant) %>%
    group_by(specialty) %>%
    summarise(n_sig = n_distinct(code), .groups = "drop")

# Build a full skeleton so every global specialty appears (0 if none significant)
summary_df <- tibble(specialty = all_specialties_global) %>%
    left_join(sig_counts, by = "specialty") %>%
    mutate(
        n_sig     = replace_na(n_sig, 0L),
        specialty_f = factor(specialty, levels = sp_levels_global)
    )

summary_panel <- ggplot(summary_df, aes(x = n_sig, y = specialty_f)) +
    geom_col(fill = PANEL_COLOUR, colour = NA, width = 0.65) +
    scale_x_continuous(
        limits = c(0, 9),
        breaks = 0:9,
        expand = expansion(mult = c(0, 0.05))
    ) +
    scale_y_discrete(limits = sp_levels_global, drop = FALSE) +
    labs(
        x     = "Number of medications\nwith significant change",
        y     = NULL,
        title = "Summary"
    ) +
    base_theme +
    theme(
        axis.text.y  = element_text(size = 10),
        axis.text.x  = element_text(size = 10),
        plot.title   = element_text(
            size   = 14,
            face   = "bold",
            color  = "black",
            hjust  = 0,
            margin = margin(b = 3)
        )
    )

# --- Arrange in 2x5 grid (2 columns, 5 rows) ---
n_cols <- 5
n_rows <- 2

# Append summary panel as the 10th slot (fills the previously empty square)
plot_list[[length(plot_list) + 1]] <- summary_panel

# Pad with spacers only if still fewer than 10 (should not be needed now)
while (length(plot_list) < n_rows * n_cols) {
    plot_list[[length(plot_list) + 1]] <- patchwork::plot_spacer()
}

grid_plot <- wrap_plots(plot_list, ncol = n_cols, nrow = n_rows) +
    plot_layout(guides = "keep")

# --- Save ---
panel_h <- max(4.0, length(all_specialties_global) * 0.30 + 2.0)
total_h  <- panel_h * n_rows + 0.5

ggsave(
    filename = paste0(outdir, "StratifiedPlot_Specialty_V4_", DATE, ".png"),
    plot     = grid_plot,
    width    = max(16, total_h * 1.6),
    height   = total_h,
    units    = "in",
    dpi      = 300
)