
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
    library(metafor)
    library(ggplot2)
})


# ============================================================
# 2. Paths
# ============================================================

# --- Date stamps used to build input file paths ---
DATE_DATA_1 <- "20260709"   # events 
DATE_DATA_2 <- "20260219"   # outcomes
TODAY       <- format(Sys.time(), "%Y%m%d")  

# --- Input ---
PATH_DOCTOR_LIST     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
PATH_EVENTS_FILE     <- paste0("/media/volume/Projects/DSGELabProject1/ProcessedData/AllDistressEvents_", DATE_DATA_1, ".parquet")
PATH_OUTCOMES_FILE   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_", DATE_DATA_2, "/ProcessedOutcomes_", DATE_DATA_2, "/processed_outcomes.parquet")
PATH_COVARIATES_FILE <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# --- Output ---
DIR_OUT             <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)
phenotype_subdir    <- function(i) file.path(DIR_OUT, paste0("DepressionDistress_Phenotype_", i))

FILE_LONG_RESULTS   <- paste0("Supplements_DepressionDistress_Long_", TODAY, ".csv")
FILE_PLOT_BASENAME  <- paste0("Plot_Supplements_DepressionDistress_", TODAY)
FILE_COMPARISON_CSV <- paste0("Supplements_DepressionDistress_PhenotypeComparison_", TODAY, ".csv")
FILE_COMPARISON_PLOT_BASENAME <- paste0("Plot_Supplements_DepressionDistress_PhenotypeComparison_", TODAY)


# ============================================================
# 3. Plotting parameters 
# ============================================================

WIN <- 3   # event-study window (years) shown in plots

# -- Export settings --
PLOT_DPI                <- 300
PLOT_WIDTH_SINGLE       <- 8
PLOT_HEIGHT_SINGLE      <- 5
PLOT_WIDTH_COMPARISON   <- 9
PLOT_HEIGHT_COMPARISON  <- 7

# -- Colors --
COLOR_SINGLE_LINE   <- "#1f77b4"      # line/point/errorbar color for per-phenotype plots
COLOR_ZERO_LINE     <- "red"            # horizontal zero-effect reference line (single plots)
COLOR_REF_LINE_CMP  <- "grey"           # horizontal/vertical reference lines (comparison plot)
PALETTE_COMPARISON  <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd", "#8c564b")  # recycled if >6 phenotypes compared

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

# List of phenotypes for analysis
PHENOTYPES <- list(

    phenotype1 = list(
        i = 1,
        name = "Recurrent depressive disorder",
        case_incl = c("F33"),
        case_excl = c("F33.4"),    # recurrent depressive disorder, currently in remission
        control_excl = c("F32", "F33", "F41", "F43", "F51", "Z73")
    ),

    phenotype2 = list(
        i = 2,
        name = "Single depressive episode",
        case_incl = c("F32"),
        case_excl = c("F33"),
        control_excl = c("F32", "F33", "F41", "F43", "F51", "Z73")
    ),

    phenotype3 = list(
        i = 3,
        name = "Distress",
        case_incl = c("F41", "F43", "F51", "Z73"),
        case_excl = c("F32", "F33"),
        control_excl = c("F32", "F33", "F41", "F43", "F51", "Z73")
    ),

    phenotype4 = list(
        i = 4,
        name = "Distress (Wide)", # test join of phenotypes 2 & 3 since they have similar effects
        case_incl = c("F32 ", "F41", "F43", "F51", "Z73"),
        case_excl = c("F33"),
        control_excl = c("F32", "F33", "F41", "F43", "F51", "Z73")
    )
)

# Subset of phenotypes to combine in the final comparison plot
compare_phenotypes <- c("phenotype1", "phenotype2", "phenotype3")

# Number of threads for parallel processing
N_THREADS <- 10
setDTthreads(N_THREADS)

# ============================================================
# 5. Load shared data (doctor list, covariates, outcomes)
#    These are the same across all phenotypes, so loaded once.
# ============================================================

doctor_ids <- fread(PATH_DOCTOR_LIST, header = FALSE)$V1

# Covariates: keep specialty and birth year
covariates <- fread(PATH_COVARIATES_FILE)
covariates[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]
covariates[SPECIALTY == "", SPECIALTY := "No specialty"]

# Outcomes: total number of prescriptions per doctor per year
outcomes <- as.data.table(read_parquet(PATH_OUTCOMES_FILE, col_select = c("DOCTOR_ID", "YEAR", "N")))

# ============================================================
# 6. Per-phenotype pipeline
# ============================================================

for (el in PHENOTYPES) {

    PHENOTYPE <- el
    subdir <- phenotype_subdir(PHENOTYPE$i)
    if (!dir.exists(subdir)) dir.create(subdir, recursive = TRUE)

    # ------------------------------------------------------------
    # 6a. Extract events and define cohort
    # ------------------------------------------------------------

    # Load events and keep only Depression/Burnout codes
    events_raw <- as.data.table(read_parquet(PATH_EVENTS_FILE))
    events_raw[, DATE := as.Date(DATE)]
    events_raw[, CODE := (CODE_ICD10)]

    # QC: add a dot after the 3rd character if it's missing (ICD-10 formatting)
    events_raw[, CODE := ifelse(
        nchar(CODE) >= 4 & substr(CODE, 4, 4) != ".",
        paste0(substr(CODE, 1, 3), ".", substr(CODE, 4, nchar(CODE))),
        CODE
    )]

    # Ids of doctors to include/exclude in the cohort based on phenotype rules
    case_incl_ids    <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_incl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
    case_excl_ids    <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
    control_excl_ids <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$control_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]

    # Extract cases for the phenotype
    events_raw <- events_raw[DOCTOR_ID %in% case_incl_ids]
    events_raw <- events_raw[!(DOCTOR_ID %in% case_excl_ids)]

    # Keep the first occurrence (of the codes of interest) for each doctor
    events_raw <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_incl, collapse = "|"), ")"), CODE),
                              .SD[which.min(DATE)], by = DOCTOR_ID]

    # Filter to doctors in our cohort, then finalize event data
    events_doctors <- events_raw[DOCTOR_ID %in% doctor_ids]
    events_doctors <- events_doctors[!is.na(DATE), EVENT_DATE := DATE]
    events_doctors <- events_doctors[, .(DOCTOR_ID, EVENT_DATE)]

    cat(sprintf("doctors with %s event: %d\n", PHENOTYPE$name, nrow(events_doctors)))

    # ------------------------------------------------------------
    # 6b. Merge events, outcomes and covariates, then QC
    # ------------------------------------------------------------

    # Left join: all outcome rows kept; controls get NA event date
    df <- left_join(outcomes, events_doctors, by = "DOCTOR_ID") %>%
        mutate(
            EVENT      = if_else(!is.na(EVENT_DATE), 1L, 0L),
            EVENT_YEAR = if_else(!is.na(EVENT_DATE), as.numeric(format(EVENT_DATE, "%Y")), NA_real_)
        ) %>%
        select(-EVENT_DATE) %>%
        as.data.table()

    # Merge covariates
    df <- covariates[df, on = "DOCTOR_ID"]
    df[, `:=`(
        AGE          = YEAR - BIRTH_YEAR,
        AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )]

    # Remove doctors whose event occurred after pension age (60)
    ids_post60 <- df[AGE_AT_EVENT > 60 & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
    df <- df[!(DOCTOR_ID %in% ids_post60) & AGE <= 60]

    # Replace missing prescription counts with 0
    df[is.na(N), N := 0]

    # Remove doctors from controls based on phenotype exclusion criteria
    df <- df[!(EVENT == 0 & DOCTOR_ID %in% control_excl_ids), ]

    # ------------------------------------------------------------
    # 6c. DiD (Callaway & Sant'Anna)
    # ------------------------------------------------------------

    # DiD variables: numeric ID, group (first treatment year), calendar year
    df[, ID := as.integer(factor(DOCTOR_ID))]
    df[, G  := fifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)]
    df[, T  := YEAR]

    # Baseline prescription rate in controls (used to express effects as % change)
    baseline <- mean(df$N[df$EVENT == 0], na.rm = TRUE)

    # Number of cases and controls for reporting
    n_cases    <- df[EVENT == 1, uniqueN(DOCTOR_ID)]
    n_controls <- df[EVENT == 0, uniqueN(DOCTOR_ID)]

    att_base <- att_gt(
        yname = "N",
        tname = "T",
        idname = "ID",
        gname = "G",
        xformla = ~ BIRTH_YEAR + SPECIALTY + SEX,
        data = df,
        est_method = "dr",
        control_group = "notyettreated",
        clustervars = "ID",
        pl = TRUE,
        cores = N_THREADS
    )
    agg <- aggte(att_base, type = "dynamic", na.rm = TRUE)
    results <- data.frame(
        n_cases    = n_cases,
        n_controls = n_controls,
        time       = agg$egt,
        att        = agg$att.egt,
        se         = agg$se.egt,
        baseline   = baseline
    )

    # Calculate relative change from baseline
    results <- results %>%
        mutate(
            rel_att    = round(100 * att / baseline, 5),
            rel_att_se = round(100 * se / baseline, 5)
        )

    # Save DiD results
    out_long_file <- file.path(subdir, FILE_LONG_RESULTS)
    write.csv(results, out_long_file, row.names = FALSE)

    # ------------------------------------------------------------
    # 6d. Plot event-study results (PNG + PDF)
    # ------------------------------------------------------------

    # Reload the results to plot, so this step also works if run in a separate session
    results_plot <- read.csv(out_long_file)
    data_plot <- results_plot %>% filter(time >= -WIN & time <= WIN)

    p <- ggplot(data_plot, aes(x = time, y = att)) +
        geom_line(color = COLOR_SINGLE_LINE) +
        geom_point() +
        geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = COLOR_SINGLE_LINE) +
        geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_ZERO_LINE) +
        labs(
            title = paste0("Results for: ", PHENOTYPE$name),
            subtitle = paste0("Cases: ", n_cases, ", Controls: ", n_controls),
            x = "Years from Event",
            y = "change in total number of prescriptions"
        ) +
        scale_x_continuous(breaks = -WIN:WIN) +
        THEME_BASE

    save_plot_png_pdf(p, subdir, FILE_PLOT_BASENAME, PLOT_WIDTH_SINGLE, PLOT_HEIGHT_SINGLE)
}


# ============================================================
# 7. Phenotype comparison plot
# ============================================================

# Helper: one text block per phenotype with its N and the rules used to build it
describe_phenotype <- function(ph, n_cases, n_controls) {
    sprintf("- %s (%d cases, %d controls)", ph$name, n_cases, n_controls)
}

# --- Step 1: collect the DiD results (saved in section 6c) for every phenotype ---
comparison_list  <- list()
description_list <- character()

for (ph_key in compare_phenotypes) {

    ph        <- PHENOTYPES[[ph_key]]
    ph_subdir <- phenotype_subdir(ph$i)
    ph_file   <- file.path(ph_subdir, FILE_LONG_RESULTS)

    ph_results <- read.csv(ph_file)
    ph_results$phenotype <- ph$name

    comparison_list[[ph_key]] <- ph_results
    description_list[ph_key] <- describe_phenotype(ph, ph_results$n_cases[1], ph_results$n_controls[1])
}

# --- Step 2: combine everything into a single data frame ready for plotting ---
comparison_df <- do.call(rbind, comparison_list)
rownames(comparison_df) <- NULL
data_plot <- comparison_df %>% filter(time >= -WIN & time <= WIN)

# CHECKPOINT: Save the combined comparison data
out_csv_file <- file.path(DIR_OUT, FILE_COMPARISON_CSV)
write.csv(data_plot, out_csv_file, row.names = FALSE)

# --- Step 3: plot & save---

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
        title    = "Phenotype comparison",
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