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
    library(ggrepel)
    library(ggnewscale)  
    library(patchwork)
    library(readr)
    library(metafor)
})


# ==============================================================================
# 1. PATHS
# ==============================================================================

DATE_DATA    <- "20260316"
TODAY        <- format(Sys.Date(), "%Y%m%d")

# --- Target medication ---
TARGET_CODE  <- "M01AH05"    
TARGET_LABEL <- "Etoricoxib"

# --- Inputs ---
events_file    <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedEvents_", DATE_DATA, "/processed_events.parquet")
outcomes_file  <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
doctor_list    <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# --- Output ---
outdir <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(outdir)) { dir.create(outdir, recursive = TRUE) }

OUTFILE_RESULTS <- paste0(outdir, "Supplements_StratifiedAnalysis_Specialty_V2_", TODAY, ".csv")
BASENAME_PLOT   <- paste0("Supplements_StratifiedAnalysis_Specialty_V2_", TODAY)

# ==============================================================================
# 2. PARAMETERS / ARGUMENTS
# ==============================================================================

N_THREADS      <- 10    
setDTthreads(N_THREADS)

# --- Cohort construction ---
BUFFER_YEARS   <- 1     # market entrance/exit buffer years
PENSION_AGE    <- 60    
N_THRESHOLD    <- 5     # empirical Bayes shrinkage threshold

# --- Per-specialty sample size requirement ---
N_MIN <- 15  

# Percentile bounds defining the two prescription-volume groups (per specialty):
# Bottom10 : <= 10th percentile of pre-event prescription volume
# Top10    : >= 90th percentile of pre-event prescription volume
BOTTOM_P <- 0.10
TOP_P    <- 0.90

# --- Event-time windows used for the pre/post fixed-effects meta-analysis ---
PRE_WINDOW  <- c(-3, -2, -1)
POST_WINDOW <- c(1, 2, 3)
META_METHOD <- "FE"

# --- Significance threshold ---
SIG_ALPHA <- 0.05

# --- Plot styling ---
COL_SIG       <- "#1B5E20"  
COL_NONSIG    <- "grey70"    
CI_MULTIPLIER <- 1.96        

# --- Plot export settings ---
PANEL_WIDTH  <- 6
PANEL_HEIGHT <- 5
PLOT_DPI     <- 300


# ==============================================================================
# 3. HELPER FUNCTIONS
# ==============================================================================

# Save a ggplot as both PNG and PDF using the same base filename
save_plot_png_pdf <- function(plot, dir, basename, width, height, dpi = PLOT_DPI, limitsize = TRUE) {
    ggsave(filename = file.path(dir, paste0(basename, ".png")),
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        limitsize = limitsize
    )
    ggsave(filename = file.path(dir, paste0(basename, ".pdf")),
        plot = plot,
        width = width,
        height = height,
        limitsize = limitsize
    )
}

base_theme <- theme_minimal(base_size = 8) +
    theme(
        panel.grid.major  = element_line(colour = "grey93", linewidth = 0.25),
        panel.grid.minor  = element_blank(),
        axis.ticks        = element_blank(),
        plot.margin       = margin(4, 6, 4, 6)
    )

# Per-specialty scatter plot: 
# baseline prescription rate (x-axis) vs. absolute change (y-axis), 
# one point per prescription-volume group (Bottom10 / Top10).

make_group_scatter_plot <- function(target_specialty, df_all) {

    df <- df_all %>%
        filter(specialty == target_specialty, !is.na(baseline), !is.na(absolute_change)) %>%
        mutate(
            ci_y_lo = absolute_change - CI_MULTIPLIER * absolute_change_se,
            ci_y_hi = absolute_change + CI_MULTIPLIER * absolute_change_se,
            label_colour = ifelse(significant, COL_SIG, COL_NONSIG)
        )

    if (nrow(df) == 0) return(NULL)

    # Symmetric axis limits (centred on 0), consistent scale on both axes
    pad <- 1.10
    x_lim <- max(abs(df$baseline), na.rm = TRUE)
    y_lim <- max(abs(df$absolute_change), na.rm = TRUE)
    ax_lim <- max(x_lim, y_lim) * pad
    if (!is.finite(ax_lim) || ax_lim == 0) ax_lim <- 1

    # Clip the Y-direction CI95% to the axis limits, flagging truncation for arrowheads
    STUB <- 0.06 * ax_lim
    df <- df %>%
        mutate(
            ci_y_lo_clip = pmax(ci_y_lo, -ax_lim),
            ci_y_hi_clip = pmin(ci_y_hi,  ax_lim),
            ci_lo_trunc  = ci_y_lo < -ax_lim,
            ci_hi_trunc  = ci_y_hi >  ax_lim
        )

    ggplot(df, aes(x = baseline, y = absolute_change, colour = significant, alpha = significant)) +

        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.35) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.35) +

        geom_segment(
            aes(xend = baseline, y = ci_y_lo_clip, yend = ci_y_hi_clip),
            linewidth = 0.35, show.legend = FALSE
        ) +

        geom_segment(
            data = df %>% filter(ci_hi_trunc),
            aes(xend = baseline, y = ci_y_hi_clip - STUB, yend = ci_y_hi_clip),
            arrow = grid::arrow(length = grid::unit(0.07, "inches"), type = "closed"),
            linewidth = 0.35, show.legend = FALSE
        ) +

        geom_segment(
            data = df %>% filter(ci_lo_trunc),
            aes(xend = baseline, y = ci_y_lo_clip + STUB, yend = ci_y_lo_clip),
            arrow = grid::arrow(length = grid::unit(0.07, "inches"), type = "closed"),
            linewidth = 0.35, show.legend = FALSE
        ) +

        geom_point(size = 2.5, shape = 16, show.legend = FALSE) +

        scale_colour_manual(values = c("FALSE" = COL_NONSIG, "TRUE" = COL_SIG), guide = "none") +
        scale_alpha_manual( values = c("FALSE" = 0.55,       "TRUE" = 0.90),    guide = "none") +

        ggnewscale::new_scale_colour() +

        ggrepel::geom_text_repel(
            data                = df,
            aes(x = baseline, y = absolute_change, label = group_label, colour = label_colour),
            size                = 3,
            segment.size        = 0.5,
            box.padding         = 0.35,
            point.padding       = 0.25,
            force               = 3,
            force_pull          = 0.6,
            max.overlaps        = Inf,
            max.time            = 2,
            max.iter            = 20000,
            min.segment.length  = 0.1,
            seed                = 42,
            inherit.aes         = FALSE,
            show.legend         = FALSE
        ) +

        scale_colour_identity() +

        scale_x_continuous(limits = c(-ax_lim, ax_lim)) +
        scale_y_continuous(limits = c(-ax_lim, ax_lim)) +
        labs(
            x     = "Baseline Prescription Rate \n(controls)",
            y     = "Change in Prescription Rate \n(before vs after event, 3 year window)",
            title = paste0(TARGET_LABEL, " - ", target_specialty)
        ) +
        base_theme +
        theme(
            plot.title = element_text(size = 12, face = "bold", hjust = 0, margin = margin(b = 3)),
            axis.title = element_text(size = 9, colour = "grey30"),
            axis.text  = element_text(size = 9, colour = "grey30")
        )
}


# ==============================================================================
# 4. LOAD REFERENCE DATA AND PREPARE COHORT (single medication: TARGET_CODE)
# ==============================================================================

event_code   <- paste0('Purch_', TARGET_CODE)
outcome_code <- TARGET_CODE

# --- STEP 1: Data loading ---
covariates <- fread(covariate_file)
covariates[, `:=`(
    SPECIALTY = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))
)]
covariates[, `:=`(
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL)
]
doctor_ids <- fread(doctor_list, header = FALSE)$V1

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
# Remove all information outside of buffered range
df_complete <- df_complete[YEAR >= buffered_min_year & YEAR <= buffered_max_year]
# Exclude events which happened before the first prescription of the outcome / or after the last one (using buffered range)
df_complete <- df_complete[is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)]

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

# Apply empirical Bayes shrinkage: 
df_model[, Y_mean := mean(Y[N >= N_THRESHOLD], na.rm = TRUE), by = DOCTOR_ID]
df_model[, Y := fifelse(
    N < N_THRESHOLD,
    ((N * Y + N_THRESHOLD * Y_mean) / (N + N_THRESHOLD)),
    Y
)]
df_model[, Y_mean := NULL]

# Replace empty string with "No Specialty" (for consistency with the specialty script)
df_model[SPECIALTY == "", SPECIALTY := "No Specialty"]

all_specialties <- sort(unique(as.character(df_model$SPECIALTY)))
cat(sprintf("\nFound %d specialties to loop over for %s.\n\n", length(all_specialties), TARGET_CODE))

# ==============================================================================
# 5. PER-SPECIALTY, BOTTOM/TOP PRESCRIPTION-VOLUME DiD PIPELINE
# ==============================================================================

master_result_list   <- list()
master_range_list    <- list()
skipped_specialties  <- character(0)

for (specialty in all_specialties) {

    df_spec <- df_model[as.character(SPECIALTY) == specialty, ]

    n_cases_spec    <- length(unique(df_spec[EVENT == 1, DOCTOR_ID]))
    n_controls_spec <- length(unique(df_spec[EVENT == 0, DOCTOR_ID]))
    cat(sprintf("Specialty '%s': %d cases, %d controls\n",
                specialty, n_cases_spec, n_controls_spec))


    # --- STEP 4: Calculate prescription group: Bottom10 / Top10 (within this specialty) ---
    # average prescription before event year for cases
    # average prescription across all years for controls
    df_spec[, prescription_tier_value := {
        event_year <- EVENT_YEAR[1]
        if (!is.na(event_year)) {
            round(mean(Ni[YEAR < event_year], na.rm = TRUE), 0)
        } else {
            round(mean(Ni, na.rm = TRUE), 0)
        }
    }, by = DOCTOR_ID]

    # Percentile cut-points across all doctors in this specialty
    doctor_tier_values <- unique(df_spec[, .(DOCTOR_ID, prescription_tier_value)])$prescription_tier_value

    if (length(unique(doctor_tier_values)) < 2) {
        cat(sprintf("  -> Skipping '%s': not enough variation in prescription volume to form groups.\n", specialty))
        skipped_specialties <- c(skipped_specialties, specialty)
        next
    }

    p_bottom <- quantile(doctor_tier_values, probs = BOTTOM_P, na.rm = TRUE)
    p_top    <- quantile(doctor_tier_values, probs = TOP_P,    na.rm = TRUE)

    # Assign groups; Bottom10 is checked first so it always wins over an
    # overlapping Top10 in case of ties collapsing the percentile bounds
    df_spec[, prescription_group := fcase(
        prescription_tier_value <= p_bottom, "Bottom10",
        prescription_tier_value >= p_top, "Top10",
        default = NA_character_
    )]
    df_spec[, prescription_group := factor(prescription_group, levels = c("Bottom10", "Top10"))]

    # Human-readable range label per group, e.g. "[0-2] , N = 12"
    # NOTE: "[0-0]" is shown as "[<1]"
    group_ranges <- df_spec[!is.na(prescription_group), .(
        range_lo = min(prescription_tier_value, na.rm = TRUE),
        range_hi = max(prescription_tier_value, na.rm = TRUE),
        n_cases  = uniqueN(DOCTOR_ID[EVENT == 1])
    ), by = prescription_group][order(prescription_group)]
    group_ranges[, range_label := ifelse(range_lo == 0 & range_hi == 0, "[<1]", paste0("[", range_lo, "-", range_hi, "]"))]
    group_ranges[, group_label := paste0(range_label, "\nN = ", n_cases)]
    group_ranges[, specialty := specialty]
    master_range_list[[specialty]] <- group_ranges

    # remove doctors that fall in neither group (i.e. the doctors strictly between
    # the bottom and top percentile bounds)
    n_pre <- length(unique(df_spec$DOCTOR_ID))
    df_spec <- df_spec[!is.na(prescription_group), ]
    n_post <- length(unique(df_spec$DOCTOR_ID))
    cat(sprintf("  Doctors outside Bottom10/Top10 removed: %d\n", n_pre - n_post))

    # Cases / controls per group
    tier_stats_split <- df_spec[, .(
        n_cases    = uniqueN(DOCTOR_ID[EVENT == 1]),
        n_controls = uniqueN(DOCTOR_ID[EVENT == 0])
    ), by = prescription_group][order(prescription_group)]
    cat("  Cases and controls per prescription group:\n")
    print(tier_stats_split)

    # --- STEP 5: Stratified DiD analysis by group (this specialty only) ---
    groups <- levels(df_spec$prescription_group)
    result_list_spec <- list()

    for (group in groups) {

        n_cases_grp    <- tier_stats_split[prescription_group == group, n_cases]
        n_controls_grp <- tier_stats_split[prescription_group == group, n_controls]

        # Skip modeling entirely for underpowered groups (record as NA, no att_gt call)
        if (n_cases_grp < N_MIN || n_controls_grp < N_MIN) {
            cat(sprintf("    Skipping model for '%s' / %s: %d cases, %d controls (< %d).\n", specialty, group, n_cases_grp, n_controls_grp, N_MIN))
            result_list_spec[[group]] <- data.frame(
                code               = TARGET_CODE,
                specialty          = specialty,
                prescription_group = group,
                baseline           = NA_real_,
                absolute_change    = NA_real_,
                absolute_change_se = NA_real_,
                relative_change    = NA_real_,
                p_value            = NA_real_,
                n_cases            = n_cases_grp,
                n_controls         = n_controls_grp
            )
            next
        }

        tryCatch({
            df_grp <- df_spec[prescription_group == group, ]

            # extract baseline prescription rate among controls
            baseline <- df_grp[df_grp$EVENT == 0, ] %>% summarise(baseline = mean(Y, na.rm = TRUE)) %>% pull(baseline)

            # prepare variables as requested by did package
            df_grp$ID <- as.integer(factor(df_grp$DOCTOR_ID))
            df_grp$G  <- ifelse(is.na(df_grp$EVENT_YEAR), 0, df_grp$EVENT_YEAR)
            df_grp$T  <- df_grp$YEAR

            set.seed(09152024)
            att_gt_res_grp <- att_gt(
                yname = "Y",
                tname = "T",
                idname = "ID",
                gname = "G",
                xformla = ~ BIRTH_YEAR + SEX,
                data = df_grp,
                est_method = "dr",
                control_group = "notyettreated",
                clustervars = "ID",
                pl = TRUE,
                cores = N_THREADS
            )

            agg_dynamic <- aggte(att_gt_res_grp, type = "dynamic", na.rm = TRUE)
            results <- data.frame(
                time = agg_dynamic$egt,
                att  = agg_dynamic$att.egt,
                se   = agg_dynamic$se.egt
            )

            # For medications results will consider ATT and SE in a 3 year window before and after event (t=0)
            before_idx <- results$time %in% PRE_WINDOW
            after_idx  <- results$time %in% POST_WINDOW

            # Meta-analysis of pre-period estimates
            pre_data <- data.frame(
                estimate = results$att[before_idx],
                se       = results$se[before_idx]
            )
            pre_meta        <- metafor::rma(yi = estimate, sei = se, data = pre_data, method = META_METHOD)
            avg_effect_before <- pre_meta$b[, 1]
            se_pre          <- pre_meta$se
            p_value_pre     <- pre_meta$pval

            # Meta-analysis of post-period estimates
            post_data <- data.frame(
                estimate = results$att[after_idx],
                se       = results$se[after_idx]
            )
            post_meta        <- metafor::rma(yi = estimate, sei = se, data = post_data, method = META_METHOD)
            avg_effect_after <- post_meta$b[, 1]
            se_post          <- post_meta$se
            p_value_post     <- post_meta$pval

            # Absolute change and relative change estimates
            absolute_change    <- avg_effect_after - avg_effect_before
            absolute_change_se <- sqrt(se_post^2 + se_pre^2)
            score_abs          <- absolute_change / absolute_change_se
            p_value_change     <- 2 * (1 - pnorm(abs(score_abs)))
            relative_change    <- ifelse(baseline != 0, (absolute_change + baseline) / baseline, NA_real_)

            result_list_spec[[group]] <- data.frame(
                code               = TARGET_CODE,
                specialty          = specialty,
                prescription_group = group,
                baseline           = round(baseline, 5),
                absolute_change    = round(absolute_change, 5),
                absolute_change_se = round(absolute_change_se, 5),
                relative_change    = round(relative_change, 5),
                p_value            = round(p_value_change, 5),
                n_cases            = n_cases_grp,
                n_controls         = n_controls_grp
            )

        }, error = function(e) {
            cat(sprintf("    Error for '%s' / group '%s': %s\n", specialty, group, e$message))
            result_list_spec[[group]] <<- data.frame(
                code               = TARGET_CODE,
                specialty          = specialty,
                prescription_group = group,
                baseline           = NA_real_,
                absolute_change    = NA_real_,
                absolute_change_se = NA_real_,
                relative_change    = NA_real_,
                p_value            = NA_real_,
                n_cases            = n_cases_grp,
                n_controls         = n_controls_grp
            )
        })
    }

    if (length(result_list_spec) > 0) {
        master_result_list[[specialty]] <- do.call(rbind, result_list_spec)
    }
}

cat(sprintf("\nSkipped %d / %d specialties entirely (underpowered or no variation): %s\n",
            length(skipped_specialties), length(all_specialties), paste(skipped_specialties, collapse = ", ")))


# ==============================================================================
# 6. COMBINE & SAVE RESULTS
# ==============================================================================

combined_results <- do.call(rbind, master_result_list)
rownames(combined_results) <- NULL

group_ranges_all <- do.call(rbind, master_range_list)
rownames(group_ranges_all) <- NULL

# Attach the human-readable group range labels
combined_results <- combined_results %>%
    left_join(
        group_ranges_all %>% transmute(specialty, prescription_group = as.character(prescription_group), group_label),
        by = c("specialty", "prescription_group")
    )

# Mask estimates below sample threshold (safety net; groups below N_MIN were already left as NA above)
combined_results <- combined_results %>%
    mutate(
        across(
            c(absolute_change, absolute_change_se, relative_change, baseline),
            ~ ifelse(is.na(n_cases) | is.na(n_controls) | n_cases < N_MIN | n_controls < N_MIN, NA_real_, .)
        ),
        significant = ifelse(!is.na(p_value) & p_value < SIG_ALPHA, TRUE, FALSE)
    )

# Keep only specialties for which at least one group was actually estimated
specialties_with_estimates <- combined_results %>%
    filter(!is.na(absolute_change)) %>%
    pull(specialty) %>%
    unique()

cat(sprintf("\n%d / %d specialties have at least one estimated group and will be exported/plotted.\n",
            length(specialties_with_estimates), length(unique(combined_results$specialty))))

combined_results <- combined_results %>%
    filter(specialty %in% specialties_with_estimates)

# Save final results (all retained specialties, one combined file)
write.csv(combined_results, OUTFILE_RESULTS, row.names = FALSE)


# ==============================================================================
# 7. SCATTER PLOTS: BASELINE VS. ABSOLUTE CHANGE BY PRESCRIPTION GROUP
# One plot per specialty (only for specialties retained above)
#
#   X-axis    : baseline prescription rate (controls) per group
#   Y-axis    : absolute change estimate (DiD) with 95% CI, per group
#   Points    : one per group (Bottom10 / Top10); colour = significant
#   Labels    : group name/range on every point
#   Ref lines : x = 0, y = 0; diagonal y = x for reference
# ==============================================================================

# reload data if running this section independently
combined_results <- read_csv(OUTFILE_RESULTS, show_col_types = FALSE)

# Only plot specialties where both estimates are available
# (one non-NA row each for Bottom10 and Top10).
specialty_row_counts <- table(combined_results$specialty[!is.na(combined_results$absolute_change)])
specialties_to_plot <- sort(names(specialty_row_counts[specialty_row_counts == 2]))
cat(sprintf("\nGenerating scatter plots for %d specialties with both estimated groups available.\n", length(specialties_to_plot)))

scatter_list <- list()

# Generate scatter plots for each specialty
for (specialty in specialties_to_plot) {
    p <- make_group_scatter_plot(specialty, combined_results)
    if (!is.null(p)) {
        scatter_list[[length(scatter_list) + 1]] <- p
    }
}

# --- PATCHWORK: combine every retained specialty's scatter plot into one grid ---
if (length(scatter_list) > 0) {

    n_plots <- length(scatter_list)
    # Pick a roughly-square grid (favouring a few more columns than rows)
    GRID_NCOL <- ceiling(sqrt(n_plots))
    GRID_NROW <- ceiling(n_plots / GRID_NCOL)
    n_slots   <- GRID_NCOL * GRID_NROW

    padded_list <- scatter_list
    while (length(padded_list) < n_slots) {
        padded_list[[length(padded_list) + 1]] <- patchwork::plot_spacer()
    }

    scatter_grid <- wrap_plots(padded_list, ncol = GRID_NCOL, nrow = GRID_NROW) +
        plot_layout(guides = "keep") +
        plot_annotation(
            title = paste0(TARGET_LABEL),
            theme = theme(plot.title = element_text(size = 16, face = "bold"))
        )

    # Save as both PNG and PDF (limitsize = FALSE: the canvas can exceed 50in
    # once there are many specialty panels)
    save_plot_png_pdf(
        plot      = scatter_grid,
        dir       = outdir,
        basename  = BASENAME_PLOT,
        width     = PANEL_WIDTH  * GRID_NCOL,
        height    = PANEL_HEIGHT * GRID_NROW,
        dpi       = PLOT_DPI,
        limitsize = FALSE
    )
}