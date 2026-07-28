# ============================================================
# Prescription patterns around depression/distress phenotypes,
# with sick-leave-adjusted (LOCF) DiD analysis
#
# Pipeline (per phenotype):
#   1. Extract events
#   2. Load sick leave periods (shared, loaded once)
#   3. Merge MONTH-resolution outcomes
#   4. LOCF: for months within a sick leave period, carry forward the N value from the month immediately before the leave
#   5. Aggregate to YEAR level
#   6. Run DiD model
#   7. Save + Plot
#
# After : build one combined comparison figure across a chosen subset of phenotypes
# ============================================================


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
    library(ggplot2)
    library(patchwork)
    library(did)
})

# ============================================================
# 2. Paths 
# ============================================================

# --- Date stamps used to build input file paths ---
DATE_DATA_1 <- "20260709"   # events / sick leave extraction date
DATE_DATA_2 <- "20250926"   # MONTH-resolution outcomes extraction date
TODAY       <- format(Sys.time(), "%Y%m%d")  

# --- Input ---
PATH_DOCTOR_LIST     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
PATH_EVENTS_FILE     <- paste0("/media/volume/Projects/DSGELabProject1/ProcessedData/AllDistressEvents_", DATE_DATA_1, ".parquet")
PATH_SICK_LEAVE_FILE <- paste0("/media/volume/Projects/DSGELabProject1/ProcessedData/all_sickleaves_doctors_", DATE_DATA_1, ".parquet")
PATH_OUTCOMES_FILE   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/Archive/Version1_Highthroughput_drop/ProcessedOutcomes_", DATE_DATA_2, "/processed_outcomes.parquet")
PATH_COVARIATES_FILE <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# --- Output ---
DIR_OUT             <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)
phenotype_subdir    <- function(i) file.path(DIR_OUT, paste0("DepressionDistress_Phenotype_", i))

FILE_LONG_RESULTS   <- paste0("Supplements_DepressionDistress_SickLeave_Long_", TODAY, ".csv")
FILE_PLOT_BASENAME  <- paste0("Plot_Supplements_DepressionDistress_SickLeave_", TODAY)
FILE_COMPARISON_CSV <- paste0("Supplements_DepressionDistress_SickLeave_PhenotypeComparison_", TODAY, ".csv")
FILE_COMPARISON_PLOT_BASENAME <- paste0("Plot_Supplements_DepressionDistress_SickLeave_PhenotypeComparison_", TODAY)

# ============================================================
# 3. Plotting parameters 
# ============================================================

WIN <- 3   # event-study window (years) shown in plots

# -- Export settings --
PLOT_DPI               <- 300
PLOT_WIDTH_SINGLE       <- 8
PLOT_HEIGHT_SINGLE      <- 6
PLOT_WIDTH_COMPARISON   <- 9
PLOT_HEIGHT_COMPARISON  <- 7

# -- Colors --
COLOR_SINGLE_LINE  <- "#1f77b4"     # line/point/errorbar color for per-phenotype plots
COLOR_ZERO_LINE    <- "red"         # horizontal zero-effect reference line (single plots)
COLOR_REF_LINE_CMP <- "grey"        # horizontal/vertical reference lines (comparison plot)
PALETTE_COMPARISON <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd", "#8c564b")  # recycled if >6 phenotypes compared

# -- Shared theme / dodge settings --
THEME_BASE          <- theme_minimal()
DODGE_WIDTH_CMP     <- 0.3   
SUBTITLE_SIZE_CMP   <- 7
SUBTITLE_LINEHEIGHT_CMP <- 1.1

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
# 4. Phenotype definitions
# ============================================================

PHENOTYPES <- list(

    phenotype1 = list(
        i = 1,
        name = "Recurrent depressive disorder",
        case_incl    = c("F33"),
        case_excl    = c("F33.4"),   # recurrent depressive disorder, currently in remission
        control_excl = c("F32", "F33", "F41", "F43", "F51", "Z73")
    ),

    phenotype2 = list(
        i = 2,
        name = "Single depressive episode",
        case_incl    = c("F32"),
        case_excl    = c("F33"),
        control_excl = c("F32", "F33", "F41", "F43", "F51", "Z73")
    ),

    phenotype3 = list(
        i = 3,
        name = "Distress",
        case_incl    = c("F41", "F43", "F51", "Z73"),
        case_excl    = c("F32", "F33"),
        control_excl = c("F32", "F33", "F41", "F43", "F51", "Z73")
    ),

    phenotype4 = list(
        i = 4,
        name = "Distress (Wide)",   # union of phenotypes 2 & 3
        case_incl    = c("F32", "F41", "F43", "F51", "Z73"),
        case_excl    = c("F33"),
        control_excl = c("F32", "F33", "F41", "F43", "F51", "Z73")
    )
)

# Subset of phenotypes shown together in the combined comparison figure
compare_phenotypes <- c("phenotype1", "phenotype2", "phenotype3")

# Number of threads for parallel processing
N_THREADS <- 10
setDTthreads(N_THREADS)

# ============================================================
# 5. Load shared data (loaded ONCE, reused across all phenotypes)
# ============================================================

doctor_ids <- fread(PATH_DOCTOR_LIST, header = FALSE)$V1

# Covariates: keep specialty, sex and birth year
covariates <- fread(PATH_COVARIATES_FILE)
covariates[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]
covariates[SPECIALTY == "", SPECIALTY := "No specialty"]

# Outcomes at MONTH resolution (needed for LOCF) -- same for every phenotype
outcomes_raw <- as.data.table(read_parquet(PATH_OUTCOMES_FILE))
outcomes_raw <- outcomes_raw[DOCTOR_ID %in% doctor_ids]

# All events (any phenotype-relevant ICD-10 code), QC'd once
events_all <- as.data.table(read_parquet(PATH_EVENTS_FILE))
events_all[, DATE := as.Date(DATE)]
events_all[, CODE := (CODE_ICD10)]
# QC: add dot after 3rd character if missing (e.g. "F334" -> "F33.4")
events_all[, CODE := ifelse(
    nchar(CODE) >= 4 & substr(CODE, 4, 4) != ".",
    paste0(substr(CODE, 1, 3), ".", substr(CODE, 4, nchar(CODE))),
    CODE
)]

# Sick leave periods (for LOCF imputation) -- same for every phenotype
# Remove partial (not full) sick leaves, i.e. benefit type = 73
sl <- as.data.table(read_parquet(PATH_SICK_LEAVE_FILE))
sl <- sl[BENEFIT_TYPE != "73"]
sl[, DATE_START := as.Date(DISABILITY_START_DATE)]
sl[, DATE_END   := as.Date(SICK_LEAVE_END)]
sl[, MONTH_START := (as.numeric(format(DATE_START, "%Y")) - 1998) * 12 +
                     as.numeric(format(DATE_START, "%m"))]
sl[, MONTH_END   := (as.numeric(format(DATE_END,   "%Y")) - 1998) * 12 +
                     as.numeric(format(DATE_END,   "%m"))]
sl_periods <- sl[!is.na(MONTH_START) & !is.na(MONTH_END), .(DOCTOR_ID, MONTH_START, MONTH_END)]
sl_periods <- sl_periods[order(DOCTOR_ID, MONTH_START)]



cat("Sick leave periods loaded:", nrow(sl_periods), "periods across", uniqueN(sl_periods$DOCTOR_ID), "doctors\n")

# ============================================================
# 6. Helper: run the full LOCF-adjusted DiD pipeline for ONE phenotype. 
# ============================================================

run_phenotype_locf_did <- function(PHENOTYPE, outcomes_raw, events_all, covariates, sl_periods, N_THREADS) {

    subdir <- phenotype_subdir(PHENOTYPE$i)
    if (!dir.exists(subdir)) dir.create(subdir, recursive = TRUE)

    cat(sprintf("\n==== Phenotype %d: %s ====\n", PHENOTYPE$i, PHENOTYPE$name))

    # ----------------------------------------------------------
    # 6a. Case / control doctor ids, from generic code patterns
    # ----------------------------------------------------------

    case_incl_ids    <- events_all[grepl(paste0("^(", paste(PHENOTYPE$case_incl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
    case_excl_ids    <- events_all[grepl(paste0("^(", paste(PHENOTYPE$case_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
    control_excl_ids <- events_all[grepl(paste0("^(", paste(PHENOTYPE$control_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]

    events_pheno <- events_all[DOCTOR_ID %in% case_incl_ids]
    events_pheno <- events_pheno[!(DOCTOR_ID %in% case_excl_ids)]

    # First occurrence of a CASE-DEFINING code per doctor.
    events_pheno <- events_pheno[grepl(paste0("^(", paste(PHENOTYPE$case_incl, collapse = "|"), ")"), CODE)]
    events_pheno <- events_pheno[order(DOCTOR_ID, DATE)][, .SD[1], by = DOCTOR_ID]

    event_info <- events_pheno[, .(DOCTOR_ID, EVENT_DATE = DATE)]
    event_info[, `:=`(
        EVENT       = 1L,
        EVENT_YEAR  = as.numeric(format(EVENT_DATE, "%Y")),
        EVENT_MONTH = (as.numeric(format(EVENT_DATE, "%Y")) - 1998) * 12 + as.numeric(format(EVENT_DATE, "%m"))
    )]

    cat("Number of case doctors:", nrow(event_info), "\n")

    # ----------------------------------------------------------
    # 6b. Merge outcomes with event info + covariates, then QC
    # ----------------------------------------------------------

    df_merged <- merge(outcomes_raw, event_info[, .(DOCTOR_ID, EVENT_DATE, EVENT, EVENT_YEAR, EVENT_MONTH)],
                        by = "DOCTOR_ID", all.x = TRUE)
    df_merged[is.na(EVENT), EVENT := 0L]

    # Drop from the control pool any doctor carrying a control_excl code
    df_merged <- df_merged[!(EVENT == 0 & DOCTOR_ID %in% control_excl_ids)]

    df_complete <- covariates[df_merged, on = "DOCTOR_ID"]
    df_complete[, `:=`(
        AGE          = YEAR - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]

    # Remove doctors whose event occurred after pension age (60)
    ids_post60  <- df_complete[AGE_AT_EVENT > 60 & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
    df_complete <- df_complete[!(DOCTOR_ID %in% ids_post60) & AGE <= 60]

    # Replace missing monthly prescription counts with 0
    df_complete[is.na(N), N := 0]

    df_complete[, `:=`(
        SPECIALTY = factor(SPECIALTY),
        SEX       = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))
    )]

    n_cases_qc    <- df_complete[EVENT == 1, uniqueN(DOCTOR_ID)]
    n_controls_qc <- df_complete[EVENT == 0, uniqueN(DOCTOR_ID)]
    cat(sprintf("After QC — Cases: %d, Controls: %d\n", n_cases_qc, n_controls_qc))

    # ----------------------------------------------------------
    # 6c. LOCF: sick-leave months carry forward the N value from
    #     the month immediately before the leave started
    # ----------------------------------------------------------

    setkey(df_complete, DOCTOR_ID, MONTH)

    # Only need sick leave periods for doctors present in this phenotype's cohort 
    sl_relevant <- sl_periods[DOCTOR_ID %in% unique(df_complete$DOCTOR_ID)]
    sl_expanded <- sl_relevant[, .(
        MONTH       = seq(MONTH_START, MONTH_END),
        MONTH_START = MONTH_START
    ), by = .(DOCTOR_ID, MONTH_START, MONTH_END)]
    sl_expanded <- unique(sl_expanded[, .(DOCTOR_ID, MONTH, MONTH_START)])

    lookup <- unique(sl_expanded[, .(DOCTOR_ID, MONTH_START)])
    lookup[, PROBE_MONTH := MONTH_START - 1L]

    n_lookup <- df_complete[, .(DOCTOR_ID, MONTH, N)]
    setkey(n_lookup, DOCTOR_ID, MONTH)

    lookup_result <- n_lookup[lookup, .(DOCTOR_ID, MONTH_START, LOCF_N = x.N),
                               on = .(DOCTOR_ID, MONTH = PROBE_MONTH), roll = TRUE]

    sl_expanded <- merge(sl_expanded, lookup_result, by = c("DOCTOR_ID", "MONTH_START"), all.x = TRUE)

    sl_impute <- sl_expanded[!is.na(LOCF_N), .(DOCTOR_ID, MONTH, LOCF_N)]
    sl_impute <- unique(sl_impute, by = c("DOCTOR_ID", "MONTH"))

    df_complete[sl_impute, N := i.LOCF_N, on = .(DOCTOR_ID, MONTH)]

    cat(sprintf("LOCF imputation: %d month-rows across %d doctors\n",
                nrow(sl_impute), uniqueN(sl_impute$DOCTOR_ID)))

    # ----------------------------------------------------------
    # 6d. Aggregate to YEAR level and build DiD variables
    # ----------------------------------------------------------

    df_did <- df_complete[, .(DOCTOR_ID, YEAR, MONTH, N, EVENT, EVENT_YEAR, SEX, BIRTH_YEAR, SPECIALTY)]
    df_did <- df_did[, .(
        N          = sum(N, na.rm = TRUE),
        EVENT      = first(EVENT),
        EVENT_YEAR = first(EVENT_YEAR),
        SEX        = first(SEX),
        BIRTH_YEAR = first(BIRTH_YEAR),
        SPECIALTY  = first(SPECIALTY)
    ), by = .(DOCTOR_ID, YEAR)]

    df_did$ID <- as.integer(factor(df_did$DOCTOR_ID))
    df_did$G  <- ifelse(is.na(df_did$EVENT_YEAR), 0, df_did$EVENT_YEAR)  
    df_did$T  <- df_did$YEAR

    # Baseline prescription rate in controls (used to express effects as % change)
    baseline <- mean(df_did$N[df_did$EVENT == 0], na.rm = TRUE)

    n_cases    <- df_did[EVENT == 1, uniqueN(DOCTOR_ID)]
    n_controls <- df_did[EVENT == 0, uniqueN(DOCTOR_ID)]
    cat(sprintf("DiD — Cases: %d, Controls: %d\n", n_cases, n_controls))

    # ----------------------------------------------------------
    # 6e. DiD model
    # ----------------------------------------------------------
    set.seed(09152024)
    att_gt_res <- att_gt(
        yname         = "N",
        tname         = "T",
        idname        = "ID",
        gname         = "G",
        xformla       = ~ BIRTH_YEAR + SEX + SPECIALTY,
        data          = df_did,
        est_method    = "dr",
        control_group = "notyettreated",
        clustervars   = "ID",
        pl            = TRUE,
        cores         = N_THREADS
    )

    agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
    results <- data.frame(
        phenotype  = PHENOTYPE$name,
        n_cases    = n_cases,
        n_controls = n_controls,
        time       = agg_dynamic$egt,
        att        = agg_dynamic$att.egt,
        se         = agg_dynamic$se.egt,
        baseline   = baseline
    )

    # Baseline & relative change estimates (effect size as % of control baseline)
    results <- results %>%
        mutate(
            rel_att    = round(100 * att / baseline, 5),
            rel_att_se = round(100 * se / baseline, 5)
        )

    # ----------------------------------------------------------
    # 6f. Save this phenotype's results + plot (PNG + PDF)
    # ----------------------------------------------------------

    out_csv <- file.path(subdir, FILE_LONG_RESULTS)
    write.csv(results, out_csv, row.names = FALSE)

    data_plot <- results %>% filter(time >= -WIN & time <= WIN)
    p <- ggplot(data_plot, aes(x = time, y = att)) +
        geom_line(color = COLOR_SINGLE_LINE) +
        geom_point() +
        geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
                      width = 0.2, color = COLOR_SINGLE_LINE) +
        geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_ZERO_LINE) +
        labs(
            title    = paste0("Sick leave-adjusted (LOCF) DiD, for ", PHENOTYPE$name),
            subtitle = paste0("Cases: ", n_cases, ", Controls: ", n_controls),
            x        = "Years from Event",
            y        = "Change in total number of prescriptions"
        ) +
        scale_x_continuous(breaks = -WIN:WIN) +
        THEME_BASE

    save_plot_png_pdf(p, subdir, FILE_PLOT_BASENAME, PLOT_WIDTH_SINGLE, PLOT_HEIGHT_SINGLE)
    list(results = results, csv_path = out_csv, subdir = subdir)
}


# ============================================================
# 7. Run the LOCF DiD pipeline for every phenotype
# ============================================================

phenotype_outputs <- list()

for (key in names(PHENOTYPES)) {

    PHENOTYPE <- PHENOTYPES[[key]]

    out <- tryCatch(
        run_phenotype_locf_did(
            PHENOTYPE    = PHENOTYPE,
            outcomes_raw = outcomes_raw,
            events_all   = events_all,
            covariates   = covariates,
            sl_periods   = sl_periods,
            N_THREADS    = N_THREADS
        ),
        error = function(e) {
            cat(sprintf("ERROR for phenotype '%s': %s\n", PHENOTYPE$name, conditionMessage(e)))
            NULL
        }
    )

    phenotype_outputs[[key]] <- out
}


# ============================================================
# 8. Combined comparison figure
# ============================================================

describe_phenotype <- function(ph, n_cases, n_controls) {sprintf("- %s (%d cases, %d controls)", ph$name, n_cases, n_controls)}
comparison_list  <- list()
description_list <- character()

for (key in compare_phenotypes) {

    out <- phenotype_outputs[[key]]
    if (is.null(out)) {
        cat(sprintf("Skipping '%s' in comparison plot (no results — likely failed above).\n", key))
        next
    }

    res <- out$results
    comparison_list[[key]] <- res
    description_list[key]  <- describe_phenotype(PHENOTYPES[[key]], res$n_cases[1], res$n_controls[1])
}

if (length(comparison_list) > 0) {

    # Prepare the exact data used for the final plot
    comparison_df <- do.call(rbind, comparison_list)
    rownames(comparison_df) <- NULL
    data_plot <- comparison_df %>% filter(time >= -WIN & time <= WIN)

    # CHECKPOINT : Save the combined comparison data 
    out_csv_file <- file.path(DIR_OUT, FILE_COMPARISON_CSV)
    write.csv(data_plot, out_csv_file, row.names = FALSE)

    # Plot
    subtitle_text       <- paste(description_list, collapse = "\n")
    ph_names            <- sapply(compare_phenotypes, function(k) PHENOTYPES[[k]]$name)
    phenotype_colors    <- setNames(rep_len(PALETTE_COMPARISON, length(ph_names)), ph_names)

    p <- ggplot(data_plot, aes(x = time, y = att, color = phenotype, group = phenotype)) +
        geom_line(linewidth = 0.8, position = position_dodge(width = DODGE_WIDTH_CMP)) +
        geom_point(size = 2, position = position_dodge(width = DODGE_WIDTH_CMP)) +
        geom_errorbar(
            aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
            width = 0.2, position = position_dodge(width = DODGE_WIDTH_CMP)
        ) +
        geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_REF_LINE_CMP) +
        geom_vline(xintercept = 0, linetype = "dashed", color = COLOR_REF_LINE_CMP) +
        scale_color_manual(values = phenotype_colors) +
        labs(
            title    = "Phenotype comparison, Sick leave-adjusted (LOCF)",
            subtitle = subtitle_text,
            x        = "Years from Event",
            y        = "Change in Total Number of Prescriptions \n(compared to controls)",
            color    = "Phenotype"
        ) +
        scale_x_continuous(breaks = -WIN:WIN) +
        THEME_BASE +
        theme(legend.position = "bottom") +
        theme(plot.subtitle = element_text(size = SUBTITLE_SIZE_CMP, lineheight = SUBTITLE_LINEHEIGHT_CMP))

    save_plot_png_pdf(p, DIR_OUT, FILE_COMPARISON_PLOT_BASENAME, PLOT_WIDTH_COMPARISON, PLOT_HEIGHT_COMPARISON)

} else {
    cat("\nNo phenotype results available — combined comparison plot was not generated.\n")
}