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
    library(ggrepel)
    library(patchwork)
    library(readr)
    library(metafor)
    library(scales)
})

##### Arguments
DATE = "20260316"
TODAY = format(Sys.Date(), "%Y%m%d")
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

                set.seed(09152024)
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
          paste0(outdir, "Supplements_StratifiedAnalysis_Specialty_", TODAY, ".csv"),
          row.names = FALSE)


# -----------------------------------------------
# SCATTER PLOT: Baseline vs. Absolute Change by Specialty
# 2x5 grid, one scatter panel per medication 
#
# Each panel:
#   X-axis : baseline prescription rate (controls), symmetric around 0, same limits across all meds
#   Y-axis : absolute change estimates (DiD), symmetric around 0, same limits across all meds
#   Points : one per specialty; colour = significant
#   Labels : specialty name on significant points
#   Ref lines: x = 0, y = 0
#
# -----------------------------------------------

# reload data if running separately from the above code block
combined_results <- read_csv(paste0(outdir, "Supplements_StratifiedAnalysis_Specialty_", TODAY, ".csv"), show_col_types = FALSE)

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

# Apply multiple-testing correction within each medication (code) separately
combined_results <- combined_results %>%
    group_by(code) %>%
    mutate(
        p_value_adj = p.adjust(p_value, method = "fdr"),
        significant = !is.na(p_value_adj) & p_value_adj < 0.05
    ) %>%
    ungroup()

# Join labels
combined_results <- combined_results %>%
    left_join(code_labels, by = c("code" = "OUTCOME_CODE")) %>%
    mutate(med_label = ifelse(!is.na(LABEL), LABEL, code), LABEL = NULL)

# Mask estimates below sample threshold
combined_results <- combined_results %>%
    mutate(
        across(
            c(absolute_change, absolute_change_se, relative_change, baseline),
            ~ ifelse(is.na(n_cases) | is.na(n_controls) | n_cases < N_MIN | n_controls < N_MIN, NA_real_, .)
        ),
        significant = ifelse(is.na(absolute_change), FALSE, significant)
    )

# -----------------------------------------------
# Shared aesthetics
# -----------------------------------------------

COL_SIG    <- "#1B5E20";   # dark green  – significant
COL_NONSIG <- "grey70";    # grey        – not significant

base_theme <- theme_minimal(base_size = 8) +
    theme(
        panel.grid.major  = element_line(colour = "grey93", linewidth = 0.25),
        panel.grid.minor  = element_blank(),
        axis.ticks        = element_blank(),
        plot.margin       = margin(4, 6, 4, 6)
    )

# -----------------------------------------------
# Per-medication scatter plot function
# -----------------------------------------------

make_scatter_plot <- function(med_code, med_label, df_all) {

    df <- df_all %>%
        filter(code == med_code, !is.na(baseline), !is.na(absolute_change)) %>%
        mutate(
            # 95% CI for absolute change (Y direction)
            ci_y_lo = absolute_change - 1.96 * absolute_change_se,
            ci_y_hi = absolute_change + 1.96 * absolute_change_se,
        )

    # Per-medication symmetric axis limits (centred on 0)
    pad <- 1.10
    x_lim <- ceiling(max(abs(df$baseline),        na.rm = TRUE) * pad * 100) / 100
    y_lim <- ceiling(max(abs(df$absolute_change), na.rm = TRUE) * pad * 100) / 100
    # Use the same scale for both axes
    ax_lim <- max(x_lim, y_lim)

    # Clip the Y-direction CI95% to the axis limits, and flag when a bound
    # was truncated so an arrowhead can be drawn to signify the segment
    # continues beyond the visible range
    STUB <- 0.06 * ax_lim   # length of the arrowhead stub segment
    df <- df %>%
        mutate(
            ci_y_lo_clip = pmax(ci_y_lo, -ax_lim),
            ci_y_hi_clip = pmin(ci_y_hi,  ax_lim),
            ci_lo_trunc  = ci_y_lo < -ax_lim,
            ci_hi_trunc  = ci_y_hi >  ax_lim
        )

    # Per-medication correlation between baseline and absolute change
    df_lab <- df %>% filter(significant)
    # Exclude points already labelled as significant, so a point that is both
    # the largest-baseline value AND significant only gets the "significant" label
    df_max_baseline <- df %>% filter(baseline == max(baseline, na.rm = TRUE), !significant)
    assoc_test <- suppressWarnings(cor.test(df$baseline, df$absolute_change, method = "pearson"))
    assoc_lab  <- sprintf(
        "r = %.2f\np = %.3g",
        unname(assoc_test$estimate),
        assoc_test$p.value
    )

    ggplot(df, aes(x = baseline, y = absolute_change,colour = significant, alpha = significant)) +

        # Diagonal reference line y = x (slope 1 through origin)
        geom_abline(slope = 1, intercept = 0, linetype = "solid", colour = "grey80", linewidth = 0.4) +

        # Zero-reference cross
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.35) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.35) +

        # Y-direction CI95% (clipped to the axis limits so it never
        # silently disappears when a bound exceeds the plotted range)
        geom_segment(
            aes(xend = baseline, y = ci_y_lo_clip, yend = ci_y_hi_clip),
            linewidth = 0.35, show.legend = FALSE
        ) +

        # Arrowhead stub: upper bound truncated by the axis
        geom_segment(
            data = df %>% filter(ci_hi_trunc),
            aes(xend = baseline, y = ci_y_hi_clip - STUB, yend = ci_y_hi_clip),
            arrow = grid::arrow(length = grid::unit(0.07, "inches"), type = "closed"),
            linewidth = 0.35, show.legend = FALSE
        ) +

        # Arrowhead stub: lower bound truncated by the axis
        geom_segment(
            data = df %>% filter(ci_lo_trunc),
            aes(xend = baseline, y = ci_y_lo_clip + STUB, yend = ci_y_lo_clip),
            arrow = grid::arrow(length = grid::unit(0.07, "inches"), type = "closed"),
            linewidth = 0.35, show.legend = FALSE
        ) +

        # Points – fixed size, colour/alpha by significance
        geom_point(size = 2.0, shape = 16, show.legend = FALSE) +

        # Estimated association between baseline and change
        geom_smooth(
            method = "lm",
            formula = y ~ x,
            se = TRUE,
            colour = "black",
            fill = "grey70",
            linewidth = 0.45,
            alpha = 0.18,
            show.legend = FALSE
        ) +

        annotate(
            "text",
            x = -ax_lim,
            y = ax_lim,
            label = assoc_lab,
            hjust = 0,
            vjust = 1,
            size = 4,
            colour = "grey20"
        ) +

        # Label the point(s) with the largest baseline value
        ggrepel::geom_text_repel(
            data           = df_max_baseline,
            aes(x = baseline, y = absolute_change, label = specialty),
            colour         = "black",
            size           = 3,
            segment.size   = 0.5,
            segment.colour = "black",
            box.padding    = 0.3,
            max.overlaps   = 20,
            min.segment.length = 0.1,
            inherit.aes    = FALSE
        ) +

        # Specialty labels for significant points
        ggrepel::geom_text_repel(
            data           = df_lab,
            aes(x = baseline, y = absolute_change, label = specialty),
            colour         = COL_SIG,
            size           = 3,
            segment.size   = 0.5,
            segment.colour = COL_SIG,
            box.padding    = 0.3,
            max.overlaps   = 20,
            min.segment.length = 0.1,
            inherit.aes    = FALSE
        ) +

        scale_colour_manual(values = c("FALSE" = COL_NONSIG, "TRUE" = COL_SIG)) +
        scale_alpha_manual( values = c("FALSE" = 0.55,       "TRUE" = 0.90)) +
        # Symmetric axes centred on 0, same extent on both sides
        scale_x_continuous(limits = c(-ax_lim, ax_lim)) +
        scale_y_continuous(limits = c(-ax_lim, ax_lim)) +
        labs(
            x     = "Baseline Prescription Rate \n(controls)",
            y     = "Change in Prescription Rate \n(before vs after event, 3 year window)",
            title = med_label
        ) +
        base_theme +
        theme(
            plot.title = element_text(size = 12, face = "bold", hjust = 0, margin = margin(b = 3)),
            axis.title = element_text(size = 8, colour = "grey30"),
            axis.text  = element_text(size = 8, colour = "grey30")
        )
}

# -----------------------------------------------
# Build plot list: 9 medications 
# -----------------------------------------------

med_codes  <- code_labels$OUTCOME_CODE[code_labels$OUTCOME_CODE %in% unique(combined_results$code)]
med_labels <- code_labels$LABEL[code_labels$OUTCOME_CODE %in% med_codes]

scatter_list <- mapply(
    FUN       = make_scatter_plot,
    med_code  = med_codes,
    med_label = med_labels,
    MoreArgs  = list(df_all = combined_results),
    SIMPLIFY  = FALSE
)

# Arrange in 2 rows x 5 columns grid (pad with empty spacers if fewer plots than grid cells)
GRID_NCOL <- 5
GRID_NROW <- 2
n_slots <- GRID_NCOL * GRID_NROW

if (length(scatter_list) > n_slots) {
    warning(sprintf(
        "scatter_list has %d plots but the grid only has %d slots (%d x %d); some plots will not be shown.",
        length(scatter_list), n_slots, GRID_NROW, GRID_NCOL
    ))
}

while (length(scatter_list) < n_slots) {
    scatter_list[[length(scatter_list) + 1]] <- patchwork::plot_spacer()
}

scatter_grid <- wrap_plots(scatter_list, ncol = GRID_NCOL, nrow = GRID_NROW) +
    plot_layout(guides = "keep")

ggsave(
    filename = paste0(outdir, "ScatterPlot_BaselineVsChange_Specialty_", TODAY, ".png"),
    plot     = scatter_grid,
    width    = 22,
    height   = 10,
    units    = "in",
    dpi      = 300
)