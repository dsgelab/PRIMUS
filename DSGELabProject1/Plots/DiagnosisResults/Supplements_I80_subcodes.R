
# ============================================================
# This script extracts results and plots for diagnosis that shows significant effect on prescription volume:
# C50 = Malignant neoplasm of breast
# I80 = Phlebitis and thrombophlebitis
# O02 = Other abnormal products of conception
# but were not included in the main analysis
# =============================================================

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
# 2. File paths and global settings
# ============================================================

DATE_DATA <- "20260427"
TODAY     <- format(Sys.Date(), "%Y%m%d")

doctor_list     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
events_file     <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_",DATE_DATA, "/ProcessedEvents_",   DATE_DATA, "/processed_events.parquet")
outcomes_file   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_",DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
covariates_file <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

outdir   <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_I80_subcodes_", TODAY, "/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

N_THREADS  <- 10
setDTthreads(N_THREADS)

# ICD-10 codes to analyze
code = "I80"
label = "Phlebitis and thrombophlebitis"

# Window size for plot
WIN <- 3

# ============================================================
# 3. Load shared data
# ============================================================

doctor_ids <- fread(doctor_list, header = FALSE)$V1

# Covariates: keep specialty and birth year
covariates <- fread(covariates_file)
covariates[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]
covariates[SPECIALTY == "", SPECIALTY := "No specialty"]

# Outcomes: total number of prescriptions per doctor per year
outcomes <- as.data.table(read_parquet(outcomes_file, col_select = c("DOCTOR_ID", "YEAR", "N")))

# ============================================================
# 4. Loop through each diagnosis code and perform DiD analysis
# ============================================================

pattern <- paste0("^", code)

cat(sprintf("Processing diagnosis: %s (%s)", label, code))

# Load events and keep only the specified codes
events_raw <- as.data.table(read_parquet(events_file))
events_raw <- events_raw[grepl(pattern, as.character(CODE), perl = TRUE), .(PATIENT_ID, CODE, DATE)]
events_raw[, DATE := as.Date(DATE)]

# Restrict to cohort doctors and take their earliest event across any matching code
events_doctors <- events_raw[PATIENT_ID %in% doctor_ids, .(EVENT_DATE = min(DATE), CODE), by = PATIENT_ID]
setnames(events_doctors, "PATIENT_ID", "DOCTOR_ID")

# Check: if code longer than 3 chars, verify it has a dot after 3rd char (e.g., I80.3)
# If no dot is present, add it
# If code longer then 4 chars, truncate to 4 chars (e.g., I80.3)
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

# Generate multiple phenotypes: 
# I80.0 = Phlebitis of superficial vessels of lower extremities (SVT)
# I80.2 = Phlebitis of deep veins of lower extremities (DVT)
# I80.3 = Unspecified phlebitis and thrombophlebitis of lower extremities
# others
events_doctors[, PHENOTYPE := fifelse(CODE == "I80.0", "Superficial",
                                            fifelse(CODE == "I80.2", "Deep",
                                                    fifelse(CODE == "I80.3", "Unspecified", "Other")))]

# Merge events, outcomes and covariates
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

# Prepare data for DiD analysis
df[, ID := as.integer(factor(DOCTOR_ID))]
df[, G  := fifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)]
df[, T  := YEAR]

# rename sex codes to labels (assume 1=male, 2=female)
df[, SEX := factor(SEX, levels = c(1,2), labels = c("Male", "Female"))]

# report number of cases and controls by phenotype
summary_phenotype <- df[, .(
    cases = uniqueN(DOCTOR_ID[EVENT == 1]),
    controls = uniqueN(DOCTOR_ID[EVENT == 0])
), by = PHENOTYPE]
cat("Cases and controls by phenotype:\n")
print(summary_phenotype)

results_list <- list()
for (phenotype_group in na.omit(unique(df$PHENOTYPE))) {
    df_phenotype <- df[(PHENOTYPE == phenotype_group | is.na(PHENOTYPE))]

    n_cases    <- df_phenotype[EVENT == 1, uniqueN(DOCTOR_ID)]
    n_controls <- df_phenotype[EVENT == 0, uniqueN(DOCTOR_ID)]
    cat(sprintf("Phenotype %s: %d doctors (%d cases, %d controls)\n", phenotype_group, n_cases + n_controls, n_cases, n_controls))

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
    agg   <- aggte(att, type = "dynamic", na.rm = TRUE)
    results <- data.frame(
        phenotype   = phenotype_group,
        time        = agg$egt, 
        att         = agg$att.egt, 
        se          = agg$se.egt,
        n_cases     = n_cases,
        n_controls  = n_controls
    )
    results_list[[phenotype_group]] <- results
}

results <- rbindlist(results_list)

# save results to CSV
output_file <- file.path(outdir, paste0("DiD_results_I80_subcodes_", TODAY, ".csv"))
fwrite(results, output_file)

# reload results for plotting
results <- fread(output_file)

# prepare subtitle text using original names
results[, phenotype_orig := phenotype]
describe_phenotype <- function(name, n_cases, n_controls) {
    sprintf(
        "- %s (%d cases, %d controls)",
        name, n_cases, n_controls
    )
}
description_list <- character()
for (i in na.omit(unique(results$phenotype_orig))) {
    name <- i
    n_cases <- results[phenotype_orig == i, unique(n_cases)]
    n_controls <- results[phenotype_orig == i, unique(n_controls)]
    description_list <- c(description_list, describe_phenotype(name, n_cases, n_controls))
}
subtitle_text <- paste(description_list, collapse = "\n")

# rename phenotype levels for plotting (legend only)
results[, phenotype := factor(phenotype, levels = c(
    "Unspecified", 
    "Superficial", 
    "Deep", 
    "Other"
), labels = c(
    "Unspecified \n(lower extremities)", 
    "Superficial \n(lower extremities)", 
    "Deep \n(lower extremities)", 
    "Other"
))]

# plot results
data_plot <- results[time >= -WIN & time <= WIN]
p <- ggplot(data_plot, aes(x = time, y = att, color = phenotype, group = phenotype)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2, position = position_dodge(width = 0.3)) +
    geom_errorbar(
        aes(ymin = att - se, ymax = att + se),
        width = 0.2, position = position_dodge(width = 0.3),
        linetype = "solid"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    scale_color_manual(values = c(
        "Unspecified \n(lower extremities)" = "#000000",
        "Superficial \n(lower extremities)" = "#ff7f0e",
        "Deep \n(lower extremities)" = "#2ca02c",
        "Other" = "#9467bd"
    )) +
    labs(
        title = "Phlebitis and Thrombophlebitis, sub-phenotype comparison",
        subtitle = subtitle_text,
        x = "Years from Event",
        y = "Change in total number of prescriptions",
        color = "Phenotype"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    theme(plot.subtitle = element_text(size = 7, lineheight = 1.1))


#save plot
plot_file <- file.path(outdir, paste0("DiD_plot_I80_subcodes_", TODAY, ".png"))
ggsave(plot_file, p, width = 9, height = 7, dpi = 300)
