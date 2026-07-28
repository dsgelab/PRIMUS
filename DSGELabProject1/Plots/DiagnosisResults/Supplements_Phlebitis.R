
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

# --- Date stamp used to build input file paths ---
DATE_DATA <- "20260427"
TODAY     <- format(Sys.Date(), "%Y%m%d")  

# --- Input ---
PATH_DOCTOR_LIST     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
PATH_EVENTS_FILE     <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_", DATE_DATA, "/ProcessedEvents_", DATE_DATA, "/processed_events.parquet")
PATH_OUTCOMES_FILE   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_", DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
PATH_COVARIATES_FILE <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# --- Output ---
DIR_OUT <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)

FILE_RESULTS_CSV    <- paste0("Supplements_Phlebitis_Subcodes_", TODAY, ".csv")
FILE_PLOT_BASENAME  <- paste0("Plot_Supplements_Phlebitis_Subcodes_", TODAY)


# ============================================================
# 3. Plotting parameters - ALL plot settings declared here
# ============================================================

WIN <- 3   # event-study window (years) shown in the plot

# -- Export settings --
PLOT_DPI    <- 300
PLOT_WIDTH  <- 9
PLOT_HEIGHT <- 7

# -- Sub-phenotype display labels  --
PHENOTYPE_LEVELS <- c(
    "Unspecified", 
    "Superficial", 
    "Deep", 
    "Other")
PHENOTYPE_LABELS <- c(
    "Unspecified \n(lower extremities)",
    "Superficial \n(lower extremities)",
    "Deep \n(lower extremities)",
    "Other"
)

# -- Colors: one per sub-phenotype --
PHENOTYPE_COLORS <- setNames(
    c("#000000", "#ff7f0e", "#2ca02c", "#9467bd"),
    PHENOTYPE_LABELS
)
COLOR_REF_LINE <- "grey"   # horizontal/vertical zero reference lines

# -- Shared theme / dodge / subtitle settings --
THEME_BASE          <- theme_minimal()
DODGE_WIDTH         <- 0.3
SUBTITLE_SIZE       <- 7
SUBTITLE_LINEHEIGHT <- 1.1

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

# ICD-10 code to analyze (all I80.* sub-codes, split into sub-phenotypes below)
DIAGNOSIS_CODE  <- "I80"
DIAGNOSIS_LABEL <- "Phlebitis and thrombophlebitis"

# ============================================================
# 5. Load shared data
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
# 6. Extract events, build cohort, classify sub-phenotypes
# ============================================================

pattern <- paste0("^", DIAGNOSIS_CODE)

cat(sprintf("Processing diagnosis: %s (%s)", DIAGNOSIS_LABEL, DIAGNOSIS_CODE))

# Load events and keep only the specified codes
events_raw <- as.data.table(read_parquet(PATH_EVENTS_FILE))
events_raw <- events_raw[grepl(pattern, as.character(CODE), perl = TRUE), .(PATIENT_ID, CODE, DATE)]
events_raw[, DATE := as.Date(DATE)]

# Restrict to cohort doctors and take their earliest event across any matching code
events_doctors <- events_raw[PATIENT_ID %in% doctor_ids, .(EVENT_DATE = min(DATE), CODE), by = PATIENT_ID]
setnames(events_doctors, "PATIENT_ID", "DOCTOR_ID")

# QC: if code longer than 3 chars, verify it has a dot after the 3rd char (e.g. I80.3);
# if no dot is present, insert one. If code longer than 4 chars, truncate to 4 chars (e.g. I80.3)
events_doctors[, CODE := {
    origs <- as.character(CODE)
    sapply(origs, function(orig) {
        if (nchar(orig) <= 3) return(substr(orig, 1, 3))
        first3 <- substr(orig, 1, 3)
        fourth <- substr(orig, 4, 4)
        if (fourth == ".") {
            # has dot: keep first digit after dot (if present)
            digit_after <- substr(orig, 5, 5)
            if (digit_after == "" || digit_after == " ") return(first3)
            return(paste0(first3, ".", substr(digit_after, 1, 1)))
        } else {
            # no dot: insert dot between 3rd char and the following digits, keep first digit
            next_digit <- substr(orig, 4, 4)
            if (next_digit == "" || next_digit == " ") return(first3)
            return(paste0(first3, ".", substr(next_digit, 1, 1)))
        }
    }, USE.NAMES = FALSE)
}]

# Classify into sub-phenotypes:
#   I80.0 = Phlebitis of superficial vessels of lower extremities (SVT)
#   I80.2 = Phlebitis of deep veins of lower extremities (DVT)
#   I80.3 = Unspecified phlebitis and thrombophlebitis of lower extremities
#   other I80.* codes -> "Other"
events_doctors[, PHENOTYPE := fifelse(CODE == "I80.0", "Superficial",
                                            fifelse(CODE == "I80.2", "Deep",
                                                    fifelse(CODE == "I80.3", "Unspecified", "Other")))]

# ============================================================
# 7. Merge events, outcomes and covariates, then QC
# ============================================================

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

# DiD variables: numeric ID, group (first treatment year), calendar year
df[, ID := as.integer(factor(DOCTOR_ID))]
df[, G  := fifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)]
df[, T  := YEAR]

# Recode sex codes to labels (assume 1 = male, 2 = female)
df[, SEX := factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))]

# Report number of cases and controls by sub-phenotype
summary_phenotype <- df[, .(
    cases    = uniqueN(DOCTOR_ID[EVENT == 1]),
    controls = uniqueN(DOCTOR_ID[EVENT == 0])
), by = PHENOTYPE]
cat("Cases and controls by phenotype:\n")
print(summary_phenotype)

# ============================================================
# 8. Run one DiD model per sub-phenotype
#    (each sub-phenotype's cases vs. the full control pool)
# ============================================================

results_list <- list()
for (phenotype_group in na.omit(unique(df$PHENOTYPE))) {
    df_phenotype <- df[(PHENOTYPE == phenotype_group | is.na(PHENOTYPE))]

    n_cases    <- df_phenotype[EVENT == 1, uniqueN(DOCTOR_ID)]
    n_controls <- df_phenotype[EVENT == 0, uniqueN(DOCTOR_ID)]
    cat(sprintf("Phenotype %s: %d doctors (%d cases, %d controls)\n", phenotype_group, n_cases + n_controls, n_cases, n_controls))

    # Baseline prescription rate in controls (used to express effects as % change)
    baseline <- mean(df_phenotype$N[df_phenotype$EVENT == 0], na.rm = TRUE)

    att <- att_gt(
        yname = "N",
        tname = "T",
        idname = "ID",
        gname = "G",
        xformla = ~ BIRTH_YEAR + SPECIALTY + SEX,
        data = df_phenotype,
        est_method = "dr",
        control_group = "notyettreated",
        clustervars = "ID",
        pl = TRUE,
        cores = N_THREADS
    )
    agg <- aggte(att, type = "dynamic", na.rm = TRUE)
    results <- data.frame(
        phenotype  = phenotype_group,
        time       = agg$egt,
        att        = agg$att.egt,
        se         = agg$se.egt,
        n_cases    = n_cases,
        n_controls = n_controls,
        baseline   = baseline
    )

    # Calculate relative change from baseline
    results <- results %>%
        mutate(
            rel_att    = round(100 * att / baseline, 5),
            rel_att_se = round(100 * se / baseline, 5)
        )
    
    # Store results for this phenotype
    results_list[[phenotype_group]] <- results
}
results <- rbindlist(results_list)

# Save combined results, cut to the event-study window
results <- results[time >= -WIN & time <= WIN]
write_csv_path <- file.path(DIR_OUT, FILE_RESULTS_CSV)
fwrite(results, write_csv_path)


# ============================================================
# 9. Prepare plot data 
# ============================================================

# Reload results
results <- fread(write_csv_path)

# Subtitle: one line per sub-phenotype with its N 
results[, phenotype_orig := phenotype]
describe_phenotype <- function(name, n_cases, n_controls) {
    sprintf("- %s (%d cases, %d controls)", name, n_cases, n_controls)
}
description_list <- character()
for (i in na.omit(unique(results$phenotype_orig))) {
    n_cases    <- results[phenotype_orig == i, unique(n_cases)]
    n_controls <- results[phenotype_orig == i, unique(n_controls)]
    description_list <- c(description_list, describe_phenotype(i, n_cases, n_controls))
}
subtitle_text <- paste(description_list, collapse = "\n")

# Rename phenotype levels for plotting (legend labels only; see Section 3 for the mapping)
results[, phenotype := factor(phenotype, levels = PHENOTYPE_LEVELS, labels = PHENOTYPE_LABELS)]

# Plot
p <- ggplot(results, aes(x = time, y = att, color = phenotype, group = phenotype)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = DODGE_WIDTH)) +
    geom_point(size = 2, position = position_dodge(width = DODGE_WIDTH)) +
    geom_errorbar(
        aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
        width = 0.2, position = position_dodge(width = DODGE_WIDTH),
        linetype = "solid"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_REF_LINE) +
    geom_vline(xintercept = 0, linetype = "dashed", color = COLOR_REF_LINE) +
    scale_color_manual(values = PHENOTYPE_COLORS) +
    labs(
        title    = "Phlebitis and Thrombophlebitis, Sub-phenotype Comparison",
        subtitle = subtitle_text,
        x        = "Years from Event",
        y        = "Change in Total Number of Prescriptions \n(compared to controls)",
        color    = "Phenotype"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    THEME_BASE +
    theme(legend.position = "bottom") +
    theme(plot.subtitle = element_text(size = SUBTITLE_SIZE, lineheight = SUBTITLE_LINEHEIGHT))

save_plot_png_pdf(p, DIR_OUT, FILE_PLOT_BASENAME, PLOT_WIDTH, PLOT_HEIGHT)