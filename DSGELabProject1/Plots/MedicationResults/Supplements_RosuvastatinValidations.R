# ==============================================================================
# 1. Libraries
# ==============================================================================

.libPaths("/shared-directory/sd-tools/apps/R/lib/")

suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(did)
    library(ggplot2)
    library(gridExtra)
})


# ==============================================================================
# 2. Paths
# ==============================================================================

DATE <- "20260920"

# --- Inputs ---
events_file    <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedEvents_",   DATE, "/processed_events.parquet")
outcomes_file  <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedOutcomes_", DATE, "/processed_outcomes.parquet")
doctor_list    <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# --- Outputs (plots only) ---
outdir <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

file_plot_A        <- paste0(outdir, "Supplements_RosuvastatinValidation_Panel_A_", DATE, ".png")
file_plot_B        <- paste0(outdir, "Supplements_RosuvastatinValidation_Panel_B_", DATE, ".png")
file_plot_C        <- paste0(outdir, "Supplements_RosuvastatinValidation_Panel_C_", DATE, ".png")
file_plot_D        <- paste0(outdir, "Supplements_RosuvastatinValidation_Panel_D_", DATE, ".png")
file_plot_combined <- paste0(outdir, "Supplements_RosuvastatinValidation_", DATE, ".png")


# ==============================================================================
# 3. Parameters
# ==============================================================================

# -- Event and outcomes --
FOCAL_CODE       <- "C10AA07"                     # rosuvastatin
ATC_PREFIX       <- substr(FOCAL_CODE, 1, 5)      # "C10AA": statins
COMPARATOR_CODES <- c("C10AA01", "C10AA05")       # simvastatin, atorvastatin
OUTCOME_CODES    <- unique(c(FOCAL_CODE, COMPARATOR_CODES))   # focal drug first: positive control
SECOND_EVENT_CODE <- "C10AA05"                    # atorvastatin: event of panel D (panel C: FOCAL_CODE)

# -- Sample construction --
WINDOW_CODES   <- OUTCOME_CODES   # drugs defining the shared study window, panels C and D (panel B: each drug its own range)
BUFFER_YEARS   <- 1               # drop first/last year of drug availability
PENSION_AGE    <- 60              # follow-up ends at this age
N_THRESHOLD    <- 5               # empirical-Bayes shrinkage threshold (DiD models)

# -- Follow-up window (doctor level) --
FOLLOW_UP_MIN_DATE <- as.Date("1998-01-01")
FOLLOW_UP_MAX_DATE <- as.Date("2022-12-31")

# -- DiD model --
MODEL_COVARIATES <- ~ BIRTH_YEAR + SEX + SPECIALTY
EST_METHOD       <- "dr"              # doubly robust
CONTROL_GROUP    <- "notyettreated"
SEED             <- 09152024

# -- Compute --
N_THREADS <- 10
setDTthreads(N_THREADS)

# -- Plotting --
PLOT_WINDOW         <- c(-3, 3)   # event-time window shown in the DiD panels (B, C, D)
CI_MULTIPLIER       <- 1.96       # set to 1 for +/- 1 SE bars
DODGE_WIDTH         <- 0.3
FIG_WIDTH_SINGLE    <- 10
FIG_HEIGHT_SINGLE   <- 6
FIG_WIDTH_COMBINED  <- 14
FIG_HEIGHT_COMBINED <- 12
FIG_DPI             <- 300

# -- Medication names and colours (whole ATC class) --
medication_names <- c(
    "C10AA01" = "simvastatin",
    "C10AA02" = "lovastatin",
    "C10AA03" = "pravastatin",
    "C10AA04" = "fluvastatin",
    "C10AA05" = "atorvastatin",
    "C10AA06" = "cerivastatin",
    "C10AA07" = "rosuvastatin"
)

palette_medications <- c(
    "simvastatin"  = "#E63946",
    "lovastatin"   = "#9B59B6",
    "pravastatin"  = "#AF7AC5",
    "fluvastatin"  = "#F4A261",
    "atorvastatin" = "#3498DB",
    "cerivastatin" = "#16A085",
    "rosuvastatin" = "#000000"
)
palette_outcomes <- palette_medications[medication_names[OUTCOME_CODES]]   # DiD panels subset


# ==============================================================================
# 4. Functions
# ==============================================================================

fill_gaps_with_0s <- function(dt) {
  
    # 1. Get each doctor's follow-up window
    ranges <- dt[, .(min_year = FOLLOW_UP_START_YEAR[1], max_year = FOLLOW_UP_END_YEAR[1]), by = DOCTOR_ID]
    # 2. Build the year skeleton for each doctor
    skeleton <- ranges[, .(YEAR = seq(min_year, max_year)), by = DOCTOR_ID]
    # 3. Join original data onto the skeleton
    setkey(dt, DOCTOR_ID, YEAR)
    setkey(skeleton, DOCTOR_ID, YEAR)
    filled <- dt[skeleton]

    # 4. Zero-fill missing values (N and Y columns)
    filled[, N := fifelse(is.na(N), 0, N)]
    filled[, Ni := fifelse(is.na(Ni), 0, Ni)]
    filled[, Y := fifelse(is.na(Y), 0, Y)]
    # 5. Carry forward "fixed" covariate columns, but not AGE
    fixed_cols <- setdiff(names(dt), c("DOCTOR_ID", "YEAR", "N", "Ni", "Y", "AGE"))
    if (length(fixed_cols) > 0) {
      filled[, (fixed_cols) := lapply(.SD, function(x) {
          val <- x[!is.na(x)][1]
          fifelse(is.na(x), val, x)
      }), .SDcols = fixed_cols, by = DOCTOR_ID]
    }
    # 6. Recompute AGE for every row
    filled[, AGE := YEAR - BIRTH_YEAR]  

  return(filled)
}

# ==============================================================================
# 5. Load reference data (shared with Part 2)
# ==============================================================================

# --- Validated doctor list ---
doctor_ids <- fread(doctor_list, header = FALSE)$V1

# --- Doctor characteristics ---
covariates_dt <- fread(covariate_file)
covariates_dt[, `:=`(
    SPECIALTY      = as.character(INTERPRETATION),
    BIRTH_YEAR     = as.numeric(substr(BIRTH_DATE, 1, 4)),
    LICENSE_START  = as.Date(START_DATE),
    LICENSE_END    = as.Date(END_DATE),
    BIRTH_DATE     = as.Date(BIRTH_DATE),
    DEATH_DATE     = as.Date(DEATH_DATE),
    INTERPRETATION = NULL
)]

# --- Follow-up window per doctor ---
covariates_dt[, `:=`(
    FOLLOW_UP_START = pmax(FOLLOW_UP_MIN_DATE, LICENSE_START, na.rm = TRUE),
    FOLLOW_UP_END   = pmin(FOLLOW_UP_MAX_DATE, LICENSE_END,
                           BIRTH_DATE + PENSION_AGE * 365.25, DEATH_DATE, na.rm = TRUE)
)]
covariates_dt[, `:=`(
    FOLLOW_UP_START_YEAR = as.integer(format(FOLLOW_UP_START, "%Y")),
    FOLLOW_UP_END_YEAR   = as.integer(format(FOLLOW_UP_END,   "%Y"))
)]

# --- Events ---
events_raw <- as.data.table(read_parquet(events_file))
events_raw[, CODE := as.character(CODE)]

# --- Outcome file schema: check the declared outcomes exist ---
outcomes_schema <- open_dataset(outcomes_file)$schema$names
missing_outcomes <- OUTCOME_CODES[!paste0("Y_", OUTCOME_CODES) %in% outcomes_schema]
if (length(missing_outcomes) > 0) {
    stop("Outcome columns missing from the outcome file for: ", paste(missing_outcomes, collapse = ", "))
}


# ==============================================================================
# 6. Prescription landscape: average prescriptions per doctor-year, per drug
# ==============================================================================

# Class drugs that are named above and present in the outcome file
landscape_codes <- names(medication_names)[
    startsWith(names(medication_names), ATC_PREFIX) &
    paste0("N_", names(medication_names)) %in% outcomes_schema
]

landscape_results <- list()

for (code_i in landscape_codes) {

    # 6.1 Load the columns of this drug only; keep the validated doctors
    cols_i <- c("DOCTOR_ID", "YEAR", "N_general", paste0(c("N_", "first_year_", "last_year_"), code_i))
    outcomes_i <- as.data.table(read_parquet(outcomes_file, col_select = cols_i))
    outcomes_i <- outcomes_i[DOCTOR_ID %in% doctor_ids]
    landscape_i <- covariates_dt[outcomes_i, on = "DOCTOR_ID"]

    # 6.2 Availability of the drug, with a buffer: the first and last year are partial
    original_min_year <- suppressWarnings(min(landscape_i[[paste0("first_year_", code_i)]], na.rm = TRUE))
    original_max_year <- suppressWarnings(max(landscape_i[[paste0("last_year_",  code_i)]], na.rm = TRUE))
    if (!is.finite(original_min_year) || !is.finite(original_max_year)) {
        cat(sprintf("%-10s | no prescriptions in the data — skipped\n", code_i))
        next
    }
    buffered_min_year <- original_min_year + BUFFER_YEARS
    buffered_max_year <- original_max_year - BUFFER_YEARS
    cat(sprintf("%-10s (%-12s) | availability: %d-%d | buffered: %d-%d\n",
                code_i, medication_names[code_i],
                original_min_year, original_max_year, buffered_min_year, buffered_max_year))
    landscape_i <- landscape_i[YEAR >= buffered_min_year & YEAR <= buffered_max_year]

    # 6.3 Keep only doctor-years inside the doctor's follow-up window
    landscape_i <- landscape_i[YEAR >= FOLLOW_UP_START_YEAR & YEAR <= FOLLOW_UP_END_YEAR]

    # 6.4 Average yearly prescription count of this drug
    landscape_i[, Ni := get(paste0("N_", code_i))]
    landscape_results[[code_i]] <- landscape_i[, .(
        CODE            = code_i,
        MEDICATION_NAME = medication_names[code_i],
        AVG_N           = mean(Ni, na.rm = TRUE),
        N_DOCTORS       = uniqueN(DOCTOR_ID)
    ), by = YEAR]
}

landscape_all <- rbindlist(landscape_results)


# ==============================================================================
# 7. Panel A: landscape plot
# ==============================================================================

plot_A <- ggplot(landscape_all, aes(x = YEAR, y = AVG_N, color = MEDICATION_NAME, group = MEDICATION_NAME)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = palette_medications) +
    labs(x = "Year", y = "Average number of prescriptions", color = "Medication name") +
    theme_minimal()

ggsave(file_plot_A, plot_A, width = FIG_WIDTH_SINGLE, height = FIG_HEIGHT_SINGLE, dpi = FIG_DPI)

# ==============================================================================
# 8. Shared inputs for the DiD models (independent of the event)
# ==============================================================================

# --- 8.1 Load every outcome column in one pass; keep the validated doctors ---
codes_needed <- unique(c(OUTCOME_CODES, WINDOW_CODES))
outcome_cols <- unique(c(
    "DOCTOR_ID", "YEAR", "N_general",
    paste0("N_",          codes_needed),
    paste0("Y_",          codes_needed),
    paste0("first_year_", codes_needed),
    paste0("last_year_",  codes_needed)
))
outcomes_all <- as.data.table(read_parquet(outcomes_file, col_select = outcome_cols))
outcomes_all <- outcomes_all[DOCTOR_ID %in% doctor_ids]

# --- 8.2 Market availability of each drug (used to define the study windows) ---
availability <- rbindlist(lapply(codes_needed, function(code_i) {
    data.table(
        code       = code_i,
        label      = medication_names[code_i],
        first_year = min(outcomes_all[[paste0("first_year_", code_i)]], na.rm = TRUE),
        last_year  = max(outcomes_all[[paste0("last_year_",  code_i)]], na.rm = TRUE)
    )
}))
print(availability)

# --- 8.3 Add doctor characteristics and age ---
outcomes_cov <- covariates_dt[outcomes_all, on = "DOCTOR_ID"]
outcomes_cov[, `:=`(
    AGE         = YEAR - BIRTH_YEAR,
    AGE_IN_2023 = 2023 - BIRTH_YEAR
)]


# ==============================================================================
# 9. DiD model specifications
# ==============================================================================

# One row = one DiD model (event medication, outcome medication).
#   B: every medication is its own event AND outcome
#   C: shared event = first rosuvastatin purchase, all outcomes (effect on any statin)
#   D: shared event = first atorvastatin purchase, all outcomes (effect on any statin)
model_specs <- rbind(
    data.table(PANEL = "B", EVENT_CODE = OUTCOME_CODES,     OUTCOME_CODE = OUTCOME_CODES),
    data.table(PANEL = "C", EVENT_CODE = FOCAL_CODE,        OUTCOME_CODE = OUTCOME_CODES),
    data.table(PANEL = "D", EVENT_CODE = SECOND_EVENT_CODE, OUTCOME_CODE = OUTCOME_CODES)
)
print(model_specs)


# ==============================================================================
# 10. One DiD model per specification
# ==============================================================================

did_results_list <- list()

for (i in seq_len(nrow(model_specs))) {

    panel_i        <- model_specs$PANEL[i]
    event_code     <- model_specs$EVENT_CODE[i]
    outcome_code   <- model_specs$OUTCOME_CODE[i]
    window_codes   <- if (panel_i == "B") event_code else WINDOW_CODES   

    # --------------------------------------------------------------------------
    # 10.1 Event: first prescription of the event medication per doctor
    # --------------------------------------------------------------------------

    events_focal <- events_raw[SOURCE == "Purch" & CODE == event_code, .(PATIENT_ID, DATE)]
    setnames(events_focal, c("PATIENT_ID", "DATE"), c("DOCTOR_ID", "EVENT_DATE"))
    events_focal[, EVENT_DATE := as.Date(EVENT_DATE)]
    events_focal <- events_focal[order(DOCTOR_ID, EVENT_DATE)][, .SD[1], by = DOCTOR_ID]

    # --------------------------------------------------------------------------
    # 10.2 Analytic sample
    # --------------------------------------------------------------------------

    # Merge event onto the doctor-year panel and flag treatment
    df_analysis <- events_focal[outcomes_cov, on = "DOCTOR_ID", allow.cartesian = TRUE]
    df_analysis[, EVENT      := fifelse(!is.na(EVENT_DATE), 1L, 0L)]
    df_analysis[, EVENT_YEAR := fifelse(!is.na(EVENT_DATE), as.numeric(format(EVENT_DATE, "%Y")), NA_real_)]
    df_analysis[, EVENT_DATE := NULL]
    df_analysis[, AGE_AT_EVENT := fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)]

    # Study window: avoids bias from drugs entering / exiting the market
    study_min_year <- max(availability[code %in% window_codes, first_year]) + BUFFER_YEARS
    study_max_year <- min(availability[code %in% window_codes, last_year])  - BUFFER_YEARS

    # Doctors whose event falls outside the study window are removed entirely
    # (neither clean cases nor clean controls)
    df_analysis <- df_analysis[is.na(EVENT_YEAR) |
                               (EVENT_YEAR >= study_min_year & EVENT_YEAR <= study_max_year)]

    # Events after pension age, and rows after pension age
    events_after_pension <- df_analysis[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
    df_analysis <- df_analysis[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]

    # Model variables
    df_analysis[, `:=`(
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(SPECIALTY), ""))),
        SEX       = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))
    )]

    # --------------------------------------------------------------------------
    # 10.3 Model data: attach THIS outcome to the sample
    # --------------------------------------------------------------------------
    model_cols <- c("DOCTOR_ID", "YEAR", "BIRTH_YEAR", "SEX", "SPECIALTY",
                    "EVENT", "EVENT_YEAR", "FOLLOW_UP_START_YEAR", "FOLLOW_UP_END_YEAR")
    df_model <- df_analysis[, c(model_cols, "N_general",
                                paste0("N_", outcome_code),
                                paste0("Y_", outcome_code)), with = FALSE]
    setnames(df_model,
             c("N_general", paste0("N_", outcome_code), paste0("Y_", outcome_code)),
             c("N", "Ni", "Y"))
    df_model[, `:=`(N = as.numeric(N), Ni = as.numeric(Ni), Y = as.numeric(Y))]

    # Replace missing values within follow-up with 0s
    df_model <- df_model[FOLLOW_UP_START_YEAR <= FOLLOW_UP_END_YEAR]
    df_model <- fill_gaps_with_0s(df_model)

    # Remove all information outside of the study window
    df_model <- df_model[YEAR >= study_min_year & YEAR <= study_max_year]

    # Empirical-Bayes shrinkage of doctor-years with 0 < N < N_THRESHOLD, towards the
    # doctor's mean over years with N >= N_THRESHOLD (all years if none qualify)
    df_model[, Y_mean := {
        eligible = (N >= N_THRESHOLD)
        if (any(eligible)) {mean(Y[eligible], na.rm = TRUE)}
        else {mean(Y, na.rm = TRUE)}
    }, by = DOCTOR_ID]
    df_model[, Y := fifelse(
        (N != 0) & (N < N_THRESHOLD),
        ((N * Y + N_THRESHOLD * Y_mean) / (N + N_THRESHOLD)),
        Y
    )]
    df_model[, Y_mean := NULL]

    # Variables as required by the 'did' package
    df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))
    df_model$G  <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)
    df_model$T  <- df_model$YEAR

    # Sample size
    n_cases    <- uniqueN(df_model[EVENT == 1, DOCTOR_ID])
    n_controls <- uniqueN(df_model[EVENT == 0, DOCTOR_ID])
    cat(sprintf("Panel %s | event: %s | outcome: %s | window: %d-%d | cases: %d | controls: %d\n",
                panel_i, medication_names[event_code], medication_names[outcome_code],
                study_min_year, study_max_year, n_cases, n_controls))

    # --------------------------------------------------------------------------
    # 10.4 Estimation: Callaway & Sant'Anna DiD, clustered at doctor level
    # --------------------------------------------------------------------------
    set.seed(SEED)
    att_gt_res <- att_gt(
        yname         = "Y",
        tname         = "T",
        idname        = "ID",
        gname         = "G",
        xformla       = MODEL_COVARIATES,
        data          = df_model,
        est_method    = EST_METHOD,
        control_group = CONTROL_GROUP,
        clustervars   = "ID",
        pl            = TRUE,
        cores         = N_THREADS
    )

    # Dynamic (event-study) aggregation: average effect per year relative to the event
    agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)

    did_results_list[[i]] <- data.table(
        panel         = panel_i,
        event_code    = event_code,
        outcome_code  = outcome_code,
        outcome_label = medication_names[outcome_code],
        time          = agg_dynamic$egt,
        att           = agg_dynamic$att.egt,
        se            = agg_dynamic$se.egt,
        n_cases       = n_cases,
        n_controls    = n_controls
    )
}

results_combined <- rbindlist(did_results_list)


# ==============================================================================
# 11. Panels B, C, D: DiD event-study plots, one line per outcome medication
# ==============================================================================

panel_x_labels <- c(
    B = "Years from statin use event",
    C = paste0("Years from first ", medication_names[FOCAL_CODE], " use"),
    D = paste0("Years from first ", medication_names[SECOND_EVENT_CODE], " use")
)
panel_files <- c(B = file_plot_B, C = file_plot_C, D = file_plot_D)

plots_did <- list()

for (panel_i in c("B", "C", "D")) {

    # Plotting subset: event-time window plus confidence bounds
    data_plot <- results_combined[panel == panel_i & time >= PLOT_WINDOW[1] & time <= PLOT_WINDOW[2]]
    data_plot[, `:=`(
        ci_low        = att - CI_MULTIPLIER * se,
        ci_high       = att + CI_MULTIPLIER * se,
        outcome_label = factor(outcome_label, levels = medication_names[OUTCOME_CODES])
    )]

    plots_did[[panel_i]] <- ggplot(data_plot, aes(x = time, y = att, color = outcome_label, group = outcome_label)) +
        geom_line(linewidth = 0.8, position = position_dodge(width = DODGE_WIDTH)) +
        geom_point(size = 2,       position = position_dodge(width = DODGE_WIDTH)) +
        geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                      width = 0.2, position = position_dodge(width = DODGE_WIDTH)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
        scale_color_manual(values = palette_outcomes) +
        labs(
            x     = panel_x_labels[[panel_i]],
            y     = "Prescription Rate Difference\n(compared to controls)",
            color = "Medication name"
        ) +
        theme_minimal()

    ggsave(panel_files[[panel_i]], plots_did[[panel_i]],
           width = FIG_WIDTH_SINGLE, height = FIG_HEIGHT_SINGLE, dpi = FIG_DPI)
}


# ==============================================================================
# 12. Combined figure: 2 x 2 panels (A B / C D) with a shared legend
# ==============================================================================

plots_all <- list(A = plot_A, B = plots_did$B, C = plots_did$C, D = plots_did$D)

panel_titles <- c(
    A = "A. Statin prescription landscape",
    B = "B. Effect of statin use, on same-statin prescriptions",
    C = paste0("C. Effect of ", medication_names[FOCAL_CODE],     " use, on other statins prescriptions"),
    D = paste0("D. Effect of ", medication_names[SECOND_EVENT_CODE], " use, on other statins prescriptions")
)

grobs_all <- list()
for (panel_i in names(plots_all)) {
    grobs_all[[panel_i]] <- arrangeGrob(
        plots_all[[panel_i]] + theme(legend.position = "none"),
        top = grid::textGrob(
            panel_titles[[panel_i]],
            x = 0, hjust = 0, gp = grid::gpar(fontface = "bold", fontsize = 13)
        )
    )
}

# Legend taken from Panel A (covers the whole class)
legend <- cowplot::get_legend(plot_A + theme(legend.direction = "horizontal"))

ggsave(file_plot_combined, arrangeGrob(grobs = grobs_all, ncol = 2, bottom = legend),
       width = FIG_WIDTH_COMBINED, height = FIG_HEIGHT_COMBINED, dpi = FIG_DPI)