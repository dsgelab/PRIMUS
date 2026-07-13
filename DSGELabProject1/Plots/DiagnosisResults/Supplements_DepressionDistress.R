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

DATE_DATA_1  <- "20260709"
DATE_DATA_2  <- "20260219"

# Input
doctor_list     <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
events_file     <- paste0("/media/volume/Projects/DSGELabProject1/ProcessedData/AllDistressEvents_", DATE_DATA_1, ".parquet")
outcomes_file   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_",DATE_DATA_2, "/ProcessedOutcomes_", DATE_DATA_2, "/processed_outcomes.parquet")
covariates_file <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# Output
TODAY <- format(Sys.time(), "%Y%m%d")
outdir   <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_Distress_", TODAY, "/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

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

# Set number of threads for parallel processing
N_THREADS  <- 10
setDTthreads(N_THREADS)

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


for (el in PHENOTYPES) {

    PHENOTYPE <- el
    subdir <- paste0(outdir, "Phenotype_", PHENOTYPE$i, "/")
    if (!dir.exists(subdir)) dir.create(subdir, recursive = TRUE)

    # ============================================================
    # 4. Extract events and define cohort
    # ============================================================

    # Load events and keep only Depression/Burnout codes
    events_raw <- as.data.table(read_parquet(events_file))
    events_raw[, DATE := as.Date(DATE)]
    events_raw[, CODE := (CODE_ICD10)]

    # Extract ids of doctors that will be included / excluded in the cohort
    events_raw[, CODE := ifelse(
        nchar(CODE) >= 4 & substr(CODE, 4, 4) != ".", # QC: add dot after 3 char if not there
        paste0(substr(CODE, 1, 3), ".", substr(CODE, 4, nchar(CODE))),
        CODE
    )]
    case_incl_ids       <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_incl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
    case_excl_ids       <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
    control_excl_ids    <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$control_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]

    # Extract case for the phenotype
    events_raw <- events_raw[DOCTOR_ID %in% case_incl_ids]
    events_raw <- events_raw[!(DOCTOR_ID %in% case_excl_ids)]

    # Keep the first occurrence (of the codes of interest) for each doctor
    events_raw <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_incl, collapse = "|"), ")"), CODE), 
                            .SD[which.min(DATE)], by = DOCTOR_ID]

    # filter doctors in our cohort, then  finalize data
    events_doctors <- events_raw[DOCTOR_ID %in% doctor_ids]
    events_doctors <- events_doctors[!is.na(DATE), EVENT_DATE := DATE]
    events_doctors <- events_doctors[,.(DOCTOR_ID, EVENT_DATE)]

    cat(sprintf("doctors with %s event: %d\n", PHENOTYPE$name, nrow(events_doctors)))

    # ============================================================
    # 5. Merge events, outcomes and covariates & QC steps
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

    # Remove doctors from controls based on phenotype exclusion criteria
    df <- df[!(EVENT == 0 & DOCTOR_ID %in% control_excl_ids),]

    # ============================================================
    # 6. Define / Check strata
    # ============================================================

    # --- Birth-year groups: balanced by number of controls ---
    min_year <- min(df$BIRTH_YEAR, na.rm = TRUE)
    max_year <- max(df$BIRTH_YEAR, na.rm = TRUE)
    N_GROUPS <- 3
    
    # Default number of groups; adjust if fewer unique birth years
    control_birth_years <- df[EVENT == 0, sort(unique(BIRTH_YEAR))]
    unique_years <- control_birth_years
    n_years <- length(unique_years)

    # Count controls per birth year and compute cumulative distribution
    controls_by_year <- df[EVENT == 0, .(n_controls = uniqueN(DOCTOR_ID)), by = BIRTH_YEAR][order(BIRTH_YEAR)]
    controls_by_year[, cum := cumsum(n_controls)]
    total_controls <- sum(controls_by_year$n_controls)

    # Target cumulative cut points (evenly spaced) and derive cut years
    targets <- (1:(N_GROUPS - 1)) * (total_controls / N_GROUPS)
    internal_cuts <- c()
    for (t in targets) {
        # find first year where cumulative >= target, then cut after that year
        idx <- which(controls_by_year$cum >= t)[1]
        if (!is.na(idx) && idx < nrow(controls_by_year)) {
            cut_year <- controls_by_year$BIRTH_YEAR[idx] + 0.5
            internal_cuts <- c(internal_cuts, cut_year)
        }
    }

    # Build breaks covering full range; add small padding so integer years split cleanly
    by_breaks <- c(min_year - 1, internal_cuts, max_year + 1)
    # Build human-readable labels for groups (inclusive integer years)
    by_labels <- character()
    br <- c(min_year, sort(unique(floor(internal_cuts + 0.1))), max_year)
    for (g in seq_len(N_GROUPS)) {
        lower_year <- if (g == 1) min_year else br[g]
        upper_year <- if (g == N_GROUPS) max_year else (br[g + 1] - 1)
        by_labels <- c(by_labels, paste0(lower_year, " - ", upper_year))
    }
    
    # Create birth-year groups and ensure levels are sorted from earliest to latest
    tmp_by_group <- cut(df$BIRTH_YEAR, breaks = by_breaks, include.lowest = TRUE, labels = by_labels)
    df[, BIRTH_YEAR_GROUP := factor(as.character(tmp_by_group), levels = by_labels, ordered = TRUE)]

    cat("Cases and controls by birth-year group:\n")
    print(df[, .(
        cases = uniqueN(DOCTOR_ID[EVENT == 1]),
        controls = uniqueN(DOCTOR_ID[EVENT == 0])
    ), by = BIRTH_YEAR_GROUP])

    # --- Sex ---
    df[, SEX := factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))]

    cat("Cases and controls by sex:\n")
    print(df[, .(
        cases = uniqueN(DOCTOR_ID[EVENT == 1]),
        controls = uniqueN(DOCTOR_ID[EVENT == 0])
    ), by = SEX])

    # -- Specialty ---
    df[, SPECIALTY := factor(SPECIALTY, levels = unique(SPECIALTY))]

    cat("Cases and controls by specialty:\n")
    print(df[, .(
        cases = uniqueN(DOCTOR_ID[EVENT == 1]),
        controls = uniqueN(DOCTOR_ID[EVENT == 0])
    ), by = SPECIALTY])

    # ----- Extra ------
    # For each birth year, check how many doctor will be assigned to each group (cases vs controls)
    df_plot <- df[, .SD[1], by = DOCTOR_ID]
    df_plot <- df_plot[, .(count = uniqueN(DOCTOR_ID)), by = .(BIRTH_YEAR, EVENT)]
    df_plot[, EVENT := factor(EVENT, levels = c(0,1), labels = c("Control", "Case"))]
    df_plot <- df_plot[order(BIRTH_YEAR, EVENT)]
    
    # Calculate ratio of cases to controls by birth year
    df_ratio <- dcast(df_plot, BIRTH_YEAR ~ EVENT, value.var = "count")
    df_ratio[, RATIO := 100 * Case / (Case + Control)]

    # Left plot: counts for cases and controls
    p1 <- ggplot(df_plot, aes(x = BIRTH_YEAR, y = count, color = EVENT, group = EVENT)) +
        geom_line() + geom_point(size = 1) +
        labs(
            title = "Cases and controls by birth-year",
            subtitle = paste0("Results for: ", PHENOTYPE$name),
            x = "Year", 
            y = "Number of unique doctors", 
            color = "Group"
        ) +
        scale_x_continuous(breaks = seq(
            floor(min(df_ratio$BIRTH_YEAR, na.rm = TRUE) / 10) * 10,
            ceiling(max(df_ratio$BIRTH_YEAR, na.rm = TRUE) / 10) * 10,
            by = 10
        )) +
        theme_minimal() + 
        theme(legend.position = "top")

    # Right plot: percent of cases
    p2 <- ggplot(df_ratio, aes(x = BIRTH_YEAR, y = RATIO)) +
        geom_line(color = "black") + geom_point(size = 1, color = "black") +
        labs(
            title = "Ratio of controls to cases, by birth-year",
            subtitle = paste0("Results for: ", PHENOTYPE$name),
            x = "Year",
            y = "Percent of cases"
        ) +
        theme_minimal() +
        scale_x_continuous(breaks = seq(
            floor(min(df_ratio$BIRTH_YEAR, na.rm = TRUE) / 10) * 10,
            ceiling(max(df_ratio$BIRTH_YEAR, na.rm = TRUE) / 10) * 10,
            by = 10
        ))       

    # Combine plots side-by-side
    combined <- gridExtra::grid.arrange(p1, p2, ncol = 2)

    # Save the results & the plot 
    out_file <- file.path(subdir, paste0("Supplements_DepressionBurnout_CaseControls_ByBirthYear_", TODAY, ".csv"))
    write.csv(df_plot, out_file, row.names = FALSE)

    out_plot_file <- file.path(subdir, paste0("Plot_Supplements_DepressionBurnout_CaseControls_ByBirthYear_", TODAY, ".png"))
    ggsave(filename = out_plot_file, plot = combined, width = 10, height = 6, dpi = 300)

    # ============================================================
    # 7. Base DiD 
    # ============================================================

    # --- DiD variables: numeric ID, group (first treatment year), calendar year ---
    df[, ID := as.integer(factor(DOCTOR_ID))]
    df[, G  := fifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)]
    df[, T  := YEAR]

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
    agg  <- aggte(att_base, type = "dynamic", na.rm = TRUE)
    results <- data.frame(
        n_cases     = n_cases,
        n_controls  = n_controls,
        time        = agg$egt, 
        att         = agg$att.egt, 
        se          = agg$se.egt
    ) 

    # Save Base DiD long results
    out_long_file <- file.path(subdir, paste0("Supplements_DepressionBurnout_BaseDiD_Long_", TODAY, ".csv"))
    write.csv(results, out_long_file, row.names = FALSE)

    # -- Plot --
    # Reload the results to plot, if running this script in a separate session
    results_plot <- read.csv(out_long_file)
    data_plot <- results_plot %>% filter(time >= -WIN & time <= WIN)

    p <- ggplot(data_plot, aes(x = time, y = att)) +
        geom_line(color = "#1f77b4") +
        geom_point() +
        geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(
            title = paste0("Results for: ", PHENOTYPE$name),
            subtitle = paste0("Cases: ", n_cases, ", Controls: ", n_controls),
            x = "Years from Event",
            y = "change in total number of prescriptions"
        ) +
        scale_x_continuous(breaks = -WIN:WIN) +
        theme_minimal()

    out_plot_file <- file.path(subdir, paste0("Plot_Supplements_DepressionBurnout_BaseDiD_", TODAY, ".png"))
    ggsave(filename = out_plot_file, plot = p, width = 8, height = 5, dpi = 300)



    # ============================================================
    # 8. Stratified DiD — Sex
    # ============================================================

    sex_results <- list()
    sex_results_long <- list()

    for (val in levels(df$SEX)) {
        cat(sprintf("  Fitting: SEX = '%s'\n", val))

        tryCatch({

            # Subset
            df_sub      <- df[SEX == val,]
            n_cases     <- df_sub[EVENT == 1, uniqueN(DOCTOR_ID)]
            n_controls  <- df_sub[EVENT == 0, uniqueN(DOCTOR_ID)]
            df_sub[, ID := as.integer(factor(DOCTOR_ID))]
            xformla <- ~ BIRTH_YEAR + SPECIALTY

            # att_gt
            att_strata <- att_gt(
                yname         = "N",
                tname         = "T",
                idname        = "ID",
                gname         = "G",
                xformla       = xformla,
                data          = df_sub,
                est_method    = "dr",
                control_group = "notyettreated",
                clustervars   = "ID",
                pl            = TRUE,
                cores         = N_THREADS
            )
        
            # t=0 estimate
            agg     <- aggte(att_strata, type = "dynamic", na.rm = TRUE)
            results <- data.frame(
                time = agg$egt, 
                att = agg$att.egt, 
                se = agg$se.egt
            ) 

            t0_row  <- results[results$time == 0, ]
            t0_att <- if (nrow(t0_row) > 0) t0_row$att[1] else NA_real_
            t0_se  <- if (nrow(t0_row) > 0) t0_row$se[1] else NA_real_

            stratum_result <- data.frame(
                stratum_dimension   = "Sex",
                stratum_value       = as.character(val),
                n_cases             = n_cases,
                n_controls          = n_controls,
                drop                = round(t0_att, 5),
                se_drop             = round(t0_se, 5),    
                stringsAsFactors    = FALSE
            )

            sex_results[[length(sex_results) + 1]] <- stratum_result
            
            # Save long results
            results_long <- data.frame(
                stratum_dimension = "Sex",
                stratum_value     = as.character(val),
                time              = results$time,
                att               = results$att,
                se                = results$se,
                stringsAsFactors  = FALSE
            )
            sex_results_long[[length(sex_results_long) + 1]] <- results_long

        }, error = function(e) {
            cat(sprintf("    ERROR for SEX = '%s': %s\n", val, conditionMessage(e)))

            df_sub <- df[SEX == val]
            n_cases    <- df_sub[EVENT == 1, uniqueN(DOCTOR_ID)]
            n_controls <- df_sub[EVENT == 0, uniqueN(DOCTOR_ID)]

            stratum_result <- data.frame(
                stratum_dimension   = "Sex",
                stratum_value       = as.character(val),
                n_cases             = n_cases,
                n_controls          = n_controls,
                att                 = NA_real_,
                se                  = NA_real_,
                stringsAsFactors    = FALSE
            )
            sex_results[[length(sex_results) + 1]] <- stratum_result
        })
    }

    # Save Sex stratification results
    if (length(sex_results) > 0) {
        sex_results_df <- do.call(rbind, sex_results)
        rownames(sex_results_df) <- NULL
        sex_file <- file.path(subdir, paste0("Supplements_DepressionBurnout_Sex_", TODAY, ".csv"))
        write.csv(sex_results_df, sex_file, row.names = FALSE)
    }

    # Save Sex stratification long results
    if (length(sex_results_long) > 0) {
        sex_results_long_df <- do.call(rbind, sex_results_long)
        rownames(sex_results_long_df) <- NULL
        sex_long_file <- file.path(subdir, paste0("Supplements_DepressionBurnout_Sex_Long_", TODAY, ".csv"))
        write.csv(sex_results_long_df, sex_long_file, row.names = FALSE)
    }

    # -- Plot --
    # Reload the results to plot, if running this script in a separate session
    results_plot <- read.csv(sex_long_file)
    data_plot <- results_plot %>% filter(time >= -WIN & time <= WIN)
    
    # Create subtitle with cases and controls counts
    sex_counts <- sex_results_df %>% 
        group_by(stratum_value) %>% 
        summarise(n_cases = first(n_cases), n_controls = first(n_controls), .groups = "drop") %>%
        mutate(label = sprintf("%s: %d cases, %d controls", stratum_value, n_cases, n_controls)) %>%
        pull(label) %>%
        paste(collapse = " | ")

    p <- ggplot(data_plot, aes(x = time, y = att, color = stratum_value, group = stratum_value)) +
        geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
        geom_point(size = 2, position = position_dodge(width = 0.3)) +
        geom_errorbar(
            aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
            width = 0.2, position = position_dodge(width = 0.3)
        ) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
        labs(
            title = paste0("Results for: ", PHENOTYPE$name),
            subtitle = sex_counts,
            x = "Years from Event",
            y = "change in total number of prescriptions",
            color = "Sex"        ) +
        scale_x_continuous(breaks = -WIN:WIN) +
        theme_minimal()

    out_plot_file <- file.path(subdir, paste0("Plot_Supplements_DepressionBurnout_Sex_", TODAY, ".png"))
    ggsave(filename = out_plot_file, plot = p, width = 8, height = 5, dpi = 300)

    # ============================================================
    # 9. Stratified DiD — Birth Year Group
    # ============================================================

    birth_year_results <- list()
    birth_year_results_long <- list()

    for (val in levels(df$BIRTH_YEAR_GROUP)) {
        cat(sprintf("  Fitting: BIRTH_YEAR_GROUP = '%s'\n", val))

        tryCatch({    
            # Subset
            df_sub <- df[BIRTH_YEAR_GROUP == val]
            n_cases    <- df_sub[EVENT == 1, uniqueN(DOCTOR_ID)]
            n_controls <- df_sub[EVENT == 0, uniqueN(DOCTOR_ID)]
            df_sub[, ID := as.integer(factor(DOCTOR_ID))]
            xformla <- ~ SPECIALTY + SEX

            # att_gt
            att_strata <- att_gt(
                yname         = "N",
                tname         = "T",
                idname        = "ID",
                gname         = "G",
                xformla       = xformla,
                data          = df_sub,
                est_method    = "dr",
                control_group = "notyettreated",
                clustervars   = "ID",
                pl            = TRUE,
                cores         = N_THREADS
            )

            # t=0 estimate
            agg     <- aggte(att_strata, type = "dynamic", na.rm = TRUE)
            results <- data.frame(
                time = agg$egt, 
                att = agg$att.egt, 
                se = agg$se.egt
            ) 

            t0_row  <- results[results$time == 0, ]
            t0_att <- if (nrow(t0_row) > 0) t0_row$att[1] else NA_real_
            t0_se  <- if (nrow(t0_row) > 0) t0_row$se[1] else NA_real_

            stratum_result <- data.frame(
                stratum_dimension   = "Birth year group",
                stratum_value       = as.character(val),
                n_cases             = n_cases,
                n_controls          = n_controls,
                att                 = t0_att,
                se                  = t0_se,
                stringsAsFactors    = FALSE
            )

            birth_year_results[[length(birth_year_results) + 1]] <- stratum_result

            # Save long results
            results_long <- data.frame(
                stratum_dimension = "Birth year group",
                stratum_value     = as.character(val),
                time              = results$time,
                att               = results$att,
                se                = results$se,
                stringsAsFactors  = FALSE
            )
            birth_year_results_long[[length(birth_year_results_long) + 1]] <- results_long

        }, error = function(e) {
            cat(sprintf("    ERROR for BIRTH_YEAR_GROUP = '%s': %s\n", val, conditionMessage(e)))

            df_sub <- df[BIRTH_YEAR_GROUP == val]
            n_cases    <- df_sub[EVENT == 1, uniqueN(DOCTOR_ID)]
            n_controls <- df_sub[EVENT == 0, uniqueN(DOCTOR_ID)]

            stratum_result <- data.frame(
                stratum_dimension   = "Birth year group",
                stratum_value       = as.character(val),
                n_cases             = n_cases,
                n_controls          = n_controls,
                att                 = NA_real_,
                se                  = NA_real_,
                stringsAsFactors    = FALSE
            )
            birth_year_results[[length(birth_year_results) + 1]] <- stratum_result
        })

    }

    # Save Birth Year Group stratification results
    if (length(birth_year_results) > 0) {
        birth_year_results_df <- do.call(rbind, birth_year_results)
        rownames(birth_year_results_df) <- NULL
        birth_year_file <- file.path(subdir, paste0("Supplements_DepressionBurnout_Birth_year_group_", TODAY, ".csv"))
        write.csv(birth_year_results_df, birth_year_file, row.names = FALSE)
    }

    # Save Birth Year Group stratification long results
    if (length(birth_year_results_long) > 0) {
        birth_year_results_long_df <- do.call(rbind, birth_year_results_long)
        rownames(birth_year_results_long_df) <- NULL
        birth_year_long_file <- file.path(subdir, paste0("Supplements_DepressionBurnout_Birth_year_group_Long_", TODAY, ".csv"))
        write.csv(birth_year_results_long_df, birth_year_long_file, row.names = FALSE)
    }

    # -- Plot --
    # Reload the results to plot, if running this script in a separate session
    results_plot <- read.csv(birth_year_long_file)
    data_plot <- results_plot %>% filter(time >= -WIN & time <= WIN)

    # Create subtitle with cases and controls counts
    birth_year_counts <- birth_year_results_df %>% 
        group_by(stratum_value) %>% 
        summarise(n_cases = first(n_cases), n_controls = first(n_controls), .groups = "drop") %>%
        mutate(label = sprintf("%s: %d cases, %d controls", stratum_value, n_cases, n_controls)) %>%
        pull(label) %>%
        paste(collapse = " | ")

    p <- ggplot(data_plot, aes(x = time, y = att, color = stratum_value, group = stratum_value)) +
        geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
        geom_point(size = 2, position = position_dodge(width = 0.3)) +
        geom_errorbar(
            aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
            width = 0.2, position = position_dodge(width = 0.3)
        ) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
        labs(
            title = paste0("Results for: ", PHENOTYPE$name),
            subtitle = birth_year_counts,
            x = "Years from Event",
            y = "change in total number of prescriptions",
            color = "Birth year group"        
        ) +
        scale_x_continuous(breaks = -WIN:WIN) +
        theme_minimal()

    out_plot_file <- file.path(subdir, paste0("Plot_Supplements_DepressionBurnout_Birth_year_group_", TODAY, ".png"))
    ggsave(filename = out_plot_file, plot = p, width = 8, height = 5, dpi = 300)


}

# ============================================================
# 10. Plot phenotype comparison
# ============================================================

# Compare Base DiD results across phenotypes (edit this vector to compare a subset)
compare_phenotypes <- c("phenotype1", "phenotype2", "phenotype3")

# Helper: one text block per phenotype with its N and the rules used to build it
describe_phenotype <- function(ph, n_cases, n_controls) {
    sprintf(
        "- %s (%d cases, %d controls)",
        ph$name, n_cases, n_controls
    )
}

# --- Step 1: collect the Base DiD long results (saved in section 7) for every phenotype ---
comparison_list  <- list()
description_list <- character()

for (ph_key in compare_phenotypes) {

    ph        <- PHENOTYPES[[ph_key]]
    ph_subdir <- paste0(outdir, "Phenotype_", ph$i, "/")
    ph_file   <- file.path(ph_subdir, paste0("Supplements_DepressionBurnout_BaseDiD_Long_", TODAY, ".csv"))

    ph_results <- read.csv(ph_file)
    ph_results$phenotype <- ph$name

    comparison_list[[ph_key]]  <- ph_results
    description_list[ph_key]  <- describe_phenotype(ph, ph_results$n_cases[1], ph_results$n_controls[1])
}

# --- Step 2: combine everything into a single data frame ready for plotting ---
comparison_df <- do.call(rbind, comparison_list)
rownames(comparison_df) <- NULL

# Save the combined comparison data
out_csv_file <- file.path(outdir, paste0("Supplements_DepressionBurnout_PhenotypeComparison_", TODAY, ".csv"))
write.csv(comparison_df, out_csv_file, row.names = FALSE)

# --- Step 3: plot ---
data_plot <- comparison_df %>% filter(time >= -WIN & time <= WIN)

# Subtitle: all phenotypes' info (N + build rules) printed together, one block per phenotype
subtitle_text <- paste(description_list, collapse = "\n")

# Distinct, fixed colors for each phenotype being compared (recycled if more than 6)
palette   <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd", "#8c564b")
ph_names  <- sapply(compare_phenotypes, function(k) PHENOTYPES[[k]]$name)
phenotype_colors <- setNames(rep_len(palette, length(ph_names)), ph_names)

p <- ggplot(data_plot, aes(x = time, y = att, color = phenotype, group = phenotype)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2, position = position_dodge(width = 0.3)) +
    geom_errorbar(
        aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
        width = 0.2, position = position_dodge(width = 0.3)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    scale_color_manual(values = phenotype_colors) +
    labs(
        title    = "Phenotype comparison",
        subtitle = subtitle_text,
        x        = "Years from Event",
        y        = "change in total number of prescriptions",
        color    = "Phenotype"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    theme(plot.subtitle = element_text(size = 7, lineheight = 1.1))

out_plot_file <- file.path(outdir, paste0("Plot_Supplements_DepressionBurnout_PhenotypeComparison_", TODAY, ".png"))
ggsave(filename = out_plot_file, plot = p, width = 9, height = 7, dpi = 300)