
# ============================================================
# 1. Libraries
# ============================================================

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

# ============================================================
# 2. Paths 
# ============================================================

DATE_DATA <- "20260316"   
TODAY     <- format(Sys.Date(), "%Y%m%d")

# --- Input ---
PATH_MAIN_RESULTS    <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/Results_", DATE_DATA, "/Results_ATC_", DATE_DATA, ".csv")
PATH_EVENTS_FILE     <- "/media/volume/Projects/DSGELabProject1/DiD_Experiments/processed_events_self_prescription_20260330.parquet"
PATH_OUTCOMES_FILE   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
PATH_DOCTOR_LIST     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
PATH_COVARIATES_FILE <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# --- Output ---
DIR_OUT <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)

FILE_RESULTS_CSV    <- paste0("Supplements_StratifiedAnalysis_SelfPrescription_Results_", TODAY, ".csv")
FILE_BASENAME_PLOT  <- paste0("Supplements_StratifiedAnalysis_SelfPrescription_Plot_", TODAY)

# ============================================================
# 3. Plotting parameters 
# ============================================================

# -- Export settings --
PLOT_DPI        <- 300
PLOT_WIDTH      <- 10
PLOT_HEIGHT     <- 10

# -- Colors / theme --
TIER_LEVELS <- c("Yes", "No")  
TIER_COLORS <- c(
    "Yes" = "#FF9500", 
    "No" = "#1F77B4"
) 
COLOR_ZERO_LINE <- "grey60"
THEME_BASE <- theme_minimal(base_size = 9)

# -- Forest-plot layout constants --
TIER_DODGE      <- 0.1     # vertical offset between the "Yes"/"No" rows for each medication
STAR_THRESHOLD  <- 0.05    # tier-difference p-value below which a significance star is drawn

# -- Helper: save a ggplot as both PNG and PDF using the same base filename --
save_plot_png_pdf <- function(plot, dir, basename, width, height, dpi = PLOT_DPI) {
    ggsave(filename = file.path(dir, paste0(basename, ".png")), 
        plot = plot,
        width = width, 
        height = height, 
        dpi = dpi
    )
    ggsave(filename = file.path(dir, paste0(basename, ".pdf")), 
        plot = plot,
        width = width, 
        height = height
    )
}

# ============================================================
# 4. Global settings
# ============================================================

N_THREADS <- 10
setDTthreads(N_THREADS)

MIN_N_CASES  <- 300         
PVAL_METHOD  <- "bonferroni"
ALPHA        <- 0.05

# Market entrance/exit buffer years
BUFFER_YEARS <- 1  

# Age threshold for pension (doctors older than this age are excluded from the analysis)
PENSION_AGE  <- 60          

# empirical Bayes shrinkage threshold
N_THRESHOLD <- 5    

# Medications of interest for the forest plot: ATC code -> readable label
CODE_LABELS <- tibble(
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


# ============================================================
# 5. Load the main results table and identify significant medications
# ============================================================

dataset <- read_csv(PATH_MAIN_RESULTS, show_col_types = FALSE)
dataset <- dataset[dataset$N_CASES >= MIN_N_CASES, ]

# Apply multiple test correction (on the overall abs-change p-value)
dataset$PVAL_ADJ <- p.adjust(dataset$PVAL_ABS_CHANGE, method = PVAL_METHOD)
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ < ALPHA
dataset$SIG_TYPE <- case_when(
    dataset$SIGNIFICANT_CHANGE ~ "Significant",
    TRUE ~ "Not Significant"
)

# Extract list of significant medications to re-analyze/plot below
code_list <- dataset %>%
    filter(SIG_TYPE == "Significant") %>%
    pull(OUTCOME_CODE) %>%
    unique()

cat(sprintf("Significant medications to process: %d\n", length(code_list)))

# --- Load shared reference data ---
covariates <- fread(PATH_COVARIATES_FILE)
covariates[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]
doctor_ids <- fread(PATH_DOCTOR_LIST, header = FALSE)$V1
events_all <- as.data.table(read_parquet(PATH_EVENTS_FILE))


# ============================================================
# 6. Per-medication, per-tier pipeline
# ============================================================

results_by_code <- list()

for (code in code_list) {

    event_code    <- paste0("Purch_", code)
    outcome_code  <- code
    event_code_parts  <- strsplit(event_code, "_")[[1]]
    event_source      <- event_code_parts[1]
    event_actual_code <- event_code_parts[2]

    # ------------------------------------------------------------
    # 6a. Filter events for this medication and flag self-prescription
    # ------------------------------------------------------------

    events <- events_all[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]

    # Create a binary variable indicating self-prescription
    events[, SELF_PRESCRIPTION := ifelse(is.na(DOCTOR_ID), "Unknown", ifelse(PATIENT_ID == DOCTOR_ID, "Yes", "No"))]
    events[, DOCTOR_ID := NULL]

    # Load outcomes (N, Ni, and Y for this medication)
    outcome_cols <- c("DOCTOR_ID", "YEAR", "N_general", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
    outcomes <- as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = outcome_cols))

    # ------------------------------------------------------------
    # 6b. Merge events and outcomes, then QC
    # ------------------------------------------------------------

    events_new <- events[, .(PATIENT_ID, CODE, DATE, SELF_PRESCRIPTION)]
    events_new <- events_new[CODE == outcome_code]
    setnames(events_new, "PATIENT_ID", "DOCTOR_ID")
    # Keep only the first event per doctor, in case multiple matching codes exist
    events_new <- events_new[order(DOCTOR_ID, DATE)]
    events_new <- events_new[, .SD[1], by = DOCTOR_ID]
    # Ensure events are only for doctors in the doctor list
    outcomes_filtered <- outcomes[DOCTOR_ID %in% doctor_ids]

    df_merged <- events_new[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
    df_merged[, DATE := as.Date(DATE)]
    df_merged[, EVENT := ifelse(!is.na(DATE), 1, 0)]
    df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
    df_merged[, DATE := NULL]

    # Merge covariates
    df_complete <- covariates[df_merged, on = "DOCTOR_ID"]
    df_complete[, `:=`(
        AGE          = YEAR - BIRTH_YEAR,
        AGE_IN_2023  = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]

    # ------------------------------------------------------------
    # 6c. Trim the medication's on-market window 
    #     (avoid bias from the drug entering/exiting the market during the study period)
    # ------------------------------------------------------------

    original_min_year <- min(df_complete[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
    original_max_year <- max(df_complete[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    cat(sprintf("Original range of outcomes: %d-%d | Buffered range of outcomes: %d-%d\n",
                original_min_year, original_max_year, buffered_min_year, buffered_max_year))

    df_complete <- df_complete[YEAR >= buffered_min_year & YEAR <= buffered_max_year]
    # Exclude events which happened before the first prescription of the outcome, or after the last one
    df_complete <- df_complete[is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)]

    # ------------------------------------------------------------
    # 6d. Model data preparation
    # ------------------------------------------------------------

    # Remove doctors whose event happened after pension, and prescriptions logged after pension
    events_after_pension <- df_complete[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
    df_complete <- df_complete[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]

    df_model <- as.data.table(df_complete)[
        , `:=`(
            SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
            SEX       = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
            Y         = get(paste0("Y_", outcome_code)),
            Ni        = get(paste0("N_", outcome_code)),
            N         = N_general
        )
    ]
    # Replace missing Y (prescription ratio) values with 0s
    df_model[is.na(Y), Y := 0]

    # Empirical Bayes shrinkage
    df_model[, Y_mean := mean(Y[N >= N_THRESHOLD], na.rm = TRUE), by = DOCTOR_ID]
    df_model[, Y := fifelse(
        N < N_THRESHOLD,
        ((N * Y + N_THRESHOLD * Y_mean) / (N + N_THRESHOLD)),
        Y
    )]
    df_model[, Y_mean := NULL]

    # ------------------------------------------------------------
    # 6e. Stratified DiD: one model per self-prescription tier
    #     ("Unknown" self-prescription status is dropped)
    # ------------------------------------------------------------

    tier_results <- list()

    for (tier in TIER_LEVELS) {
        df_tier <- df_model[SELF_PRESCRIPTION == tier | is.na(SELF_PRESCRIPTION), ]

        n_cases_tier    <- length(unique(df_tier[EVENT == 1, DOCTOR_ID]))
        n_controls_tier <- length(unique(df_tier[EVENT == 0, DOCTOR_ID]))

        # DiD variables: numeric ID, group (first treatment year), calendar year
        df_tier$ID <- as.integer(factor(df_tier$DOCTOR_ID))
        df_tier$G  <- ifelse(is.na(df_tier$EVENT_YEAR), 0, df_tier$EVENT_YEAR)
        df_tier$T  <- df_tier$YEAR

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
            att  = agg_dynamic$att.egt,
            se   = agg_dynamic$se.egt
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

        tier_results[[tier]] <- data.frame(
            tier_name           = tier,
            n_cases             = n_cases_tier,
            n_controls          = n_controls_tier,
            absolute_change     = round(absolute_change, 5),
            absolute_change_se  = round(absolute_change_se, 5),
            p_value_change      = round(p_value_change, 5)
        )
    }

    results_by_code[[code]] <- do.call(rbind, tier_results)
}


# ============================================================
# 7. Combine results, pivot to wide format, test tier differences
# ============================================================

combined_results <- do.call(rbind, results_by_code) %>%
    rownames_to_column("code") %>%
    separate(code, into = c("code", "row"), sep = "\\.", extra = "drop") %>%
    select(-row)

# Pivot to wide format: one row per medication, one column set per tier
results_wide <- combined_results %>%
    pivot_wider(
        id_cols = code,
        names_from = tier_name,
        values_from = c(absolute_change, absolute_change_se, p_value_change, n_cases, n_controls),
        names_glue = "{tier_name}_{.value}"
    )

# Test if absolute changes are significantly different between tiers (two-sided z-test)
results_wide$tier_significance <- apply(results_wide, 1, function(row) {
    low_est  <- as.numeric(row["Yes_absolute_change"])
    low_se   <- as.numeric(row["Yes_absolute_change_se"])
    high_est <- as.numeric(row["No_absolute_change"])
    high_se  <- as.numeric(row["No_absolute_change_se"])

    if (!is.na(low_est) && !is.na(high_est) && !is.na(low_se) && !is.na(high_se)) {
        z_stat  <- (low_est - high_est) / sqrt(low_se^2 + high_se^2)
        p_value <- 2 * (1 - pnorm(abs(z_stat)))
        return(p_value)
    }
    return(NA)
})

# Save final results
out_results_file <- file.path(DIR_OUT, FILE_RESULTS_CSV)
write.csv(results_wide, out_results_file, row.names = FALSE)

# ============================================================
# 8. Forest plot of absolute changes by tier
# ============================================================

# Reload results, so this section also works if run in a separate session
results_wide <- read.csv(out_results_file)

# Pivot results_wide to long format: one row per (code x tier)
plot_data <- bind_rows(
    results_wide %>% transmute(code, group = "Yes", absolute_change = Yes_absolute_change, absolute_change_se = Yes_absolute_change_se, n_cases = Yes_n_cases),
    results_wide %>% transmute(code, group = "No",  absolute_change = No_absolute_change,  absolute_change_se = No_absolute_change_se,  n_cases = No_n_cases)
) %>%
    left_join(CODE_LABELS, by = c("code" = "OUTCOME_CODE")) %>%
    mutate(
        ci_lo = absolute_change - 1.96 * absolute_change_se,
        ci_hi = absolute_change + 1.96 * absolute_change_se
    )

# Order medications by mean effect (from the main results table)
label_order <- dataset %>%
    filter(OUTCOME_CODE %in% plot_data$code) %>%
    arrange(ABS_CHANGE) %>%
    pull(OUTCOME_CODE) %>%
    unique() %>%
    {tibble(code = .) %>% left_join(CODE_LABELS, by = c("code" = "OUTCOME_CODE")) %>% pull(LABEL)}

plot_data <- plot_data %>%
    mutate(
        LABEL   = factor(LABEL, levels = label_order),
        group   = factor(group, levels = TIER_LEVELS),
        y_pos   = as.numeric(LABEL) + ifelse(group == "Yes", +TIER_DODGE, -TIER_DODGE),
        n_label = paste0("N = ", n_cases)
    )

# Add significance star per medication if tiers differ
star_df <- results_wide %>%
    transmute(code, tier_pvalue = as.numeric(tier_significance)) %>%
    left_join(CODE_LABELS, by = c("code" = "OUTCOME_CODE")) %>%
    mutate(
        y_center = match(LABEL, label_order),
        star = ifelse(!is.na(tier_pvalue) & tier_pvalue < STAR_THRESHOLD, "*", "")
    )

# Compute x position for stars (just to the right of the largest CI)
star_x_base  <- max(plot_data$ci_hi, na.rm = TRUE)
star_x_range <- diff(range(c(plot_data$ci_lo, plot_data$ci_hi), na.rm = TRUE))
if (is.na(star_x_range) || star_x_range == 0) star_x_range <- abs(star_x_base) * 0.05 + 0.01
star_df <- star_df %>% mutate(x_star = star_x_base + 0.06 * star_x_range)

forest_plot <- ggplot(plot_data, aes(x = absolute_change, y = y_pos, colour = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = COLOR_ZERO_LINE, linewidth = 0.5) +
    geom_errorbarh(
        aes(xmin = ci_lo, xmax = ci_hi),
        height = 0.15, linewidth = 0.65, na.rm = TRUE
    ) +
    geom_point(size = 3, shape = 16, na.rm = TRUE) +
    geom_text(aes(y = y_pos + 0.07, label = n_label), size = 2.5, vjust = 0, na.rm = TRUE) +
    geom_text(
        data = star_df %>% filter(star != ""),
        aes(x = x_star, y = y_center, label = star),
        inherit.aes = FALSE,
        size = 5,
        fontface = "bold",
        colour = "black"
    ) +
    scale_y_continuous(
        breaks = seq_along(label_order),
        labels = label_order,
        expand = expansion(add = 0.6)
    ) +
    scale_colour_manual(
        values = TIER_COLORS,
        name   = "Self-prescription"
    ) +
    labs(
        x = "Change in Prescription Rate \n(before vs after event, 3 year window)",
        y = NULL
    ) +
    THEME_BASE +
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

save_plot_png_pdf(forest_plot, DIR_OUT, FILE_BASENAME_PLOT, PLOT_WIDTH, PLOT_HEIGHT)