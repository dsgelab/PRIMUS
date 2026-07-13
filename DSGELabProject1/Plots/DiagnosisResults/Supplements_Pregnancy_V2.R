# ------------------------------------------------
#### Libraries
# ------------------------------------------------
.libPaths("/shared-directory/sd-tools/apps/R/lib/")
suppressPackageStartupMessages({
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(ggplot2)
    library(patchwork)
    library(arrow)
    library(stringr)
    library(did)
})

# ------------------------------------------------
##### File Paths and Global Variables
# ------------------------------------------------

DATE_DATA <- "20260219"

doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
pregnancies_file = "/media/volume/Projects/DSGELabProject1/ProcessedData/AllPregnanciesEvents_20251016.csv"
relatives_file = "/media/volume/Projects/DSGELabProject1/doctors_and_relative_20250521.csv"
outcomes_file   <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_",DATE_DATA, "/ProcessedOutcomes_", DATE_DATA, "/processed_outcomes.parquet")
covariates_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
marriage_info_file = "/media/volume/Data_20250430/DVV/FD_2698_Tulokset 2024-09-11 AVIOLIITOT.csv"

DATE <- format(Sys.time(), "%Y%m%d")
outdir = paste0("/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_Pregnancy_Base_", DATE, "/")
if (!dir.exists(outdir)) {dir.create(outdir, recursive = TRUE)}

N_THREADS = 10
setDTthreads(N_THREADS) 

EVENT_CODE = "O80|O81|O82|O83|O84"
WIN = 3 

# ------------------------------------------------
##### Data Loading and Preprocessing
# ------------------------------------------------

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
outcomes = as.data.table(read_parquet(outcomes_file))
covariates = fread(covariates_file)
events = fread(pregnancies_file) 
relatives = fread(relatives_file)

# filter spouses of doctors
spouse_ids = relatives %>% filter(RELATIVE_TYPE == "SPOUSE") %>% pull(RELATIVE_ID) %>% unique()

# Filter events based on the event code
# Filter using regex (EVENT_CODE can be a regex like "AA|BB|CC" to match multiple codes)
pattern <- paste0("^(", EVENT_CODE, ")")
events <- events[grepl(pattern, as.character(ICD10_CODE), perl = TRUE), .(PATIENT_ID, ICD10_CODE, VISIT_DATE)]
events$VISIT_DATE <- as.Date(events$VISIT_DATE)
events = events[events[, .I[which.min(VISIT_DATE)], by = .(PATIENT_ID, ICD10_CODE)]$V1] # only use first event
events = events %>% 
    rename(
        # new = old
        EVENT_DATE = VISIT_DATE,
        EVENT_CODE = ICD10_CODE
    )

# Summary statistics:
# 1 Total number of pregnancies
unique_patient_ids <- length(unique(events$PATIENT_ID))
# 2. Number of IDs from doctor list with an event code
doctors_ids_with_event <- events %>%
    filter(PATIENT_ID %in% doctor_ids) %>%
    pull(PATIENT_ID) %>%
    unique()
# 3. Number of IDs from doctor spouse list with an event code
spouse_ids_with_event <- events %>%
    filter(PATIENT_ID %in% spouse_ids) %>%
    pull(PATIENT_ID) %>%
    unique()

# Generate list of IDS in the two groups
pregnancy_females = doctors_ids_with_event
pregnancy_males = relatives %>% 
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    filter(RELATIVE_ID %in% spouse_ids_with_event) %>%
    pull(DOCTOR_ID) %>%
    unique()
pregnancy_all = c(pregnancy_females, pregnancy_males)

# extract spouse event years for later use
spouse_events <- relatives %>%
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    select(DOCTOR_ID, RELATIVE_ID) %>%
    inner_join(events %>% select(PATIENT_ID, EVENT_DATE), by = c("RELATIVE_ID" = "PATIENT_ID")) %>%
    mutate(SPOUSE_EVENT_YEAR = as.numeric(format(EVENT_DATE, "%Y"))) %>%
    select(DOCTOR_ID, SPOUSE_EVENT_YEAR) %>%
    distinct()

# Merge events with outcomes
events = events %>% filter(PATIENT_ID %in% pregnancy_all) %>% rename(DOCTOR_ID = PATIENT_ID, DATE = EVENT_DATE)
df_merged = left_join(outcomes, events, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare  covariates and specialty + merge them in the main dataframe
covariates_new = covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>% #currently using interpretation of longest specialty
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete = merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left")
df_complete = df_complete %>% 
    mutate(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after60 = df_complete %>% filter(AGE_AT_EVENT > 60) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after60)) %>% # remove people which experiment the event after pension (age 60)
    filter(AGE <= 60) # remove all prescriptions done after pension (age 60)

df_model = df_complete %>%
    mutate(
        PERIOD = case_when(
            !is.na(EVENT_YEAR) & YEAR < EVENT_YEAR ~ "BEFORE",
            !is.na(EVENT_YEAR) & YEAR > EVENT_YEAR ~ "AFTER",
            is.na(EVENT_YEAR) ~ NA_character_),
        time = YEAR - EVENT_YEAR
    ) %>%
    mutate(
        PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")), # set BEFORE as reference
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))), # set no specialty as reference
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )

# Replace missing N values with 0s 
df_model[is.na(N), N := 0]

# Step 1: prepare the model data
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                       # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)  # G = group of first treatment year, 0 for never-treated
df_model$T <- df_model$YEAR    

# ---------------------------------------------------------------------------
# Split analysis for males and females
# ---------------------------------------------------------------------------

# Females
# keep female doctors & ensure cases are limited to pregnancy_females list
df_model_female <- df_model %>%
    filter(SEX == "Female") %>%
    filter(EVENT == 0 | (EVENT == 1 & DOCTOR_ID %in% pregnancy_females))

n_cases_female <- df_model_female %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls_female <- df_model_female %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()
events_per_year_female <- df_model_female[df_model_female$EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
events_year_str_female <- paste0(events_per_year_female$EVENT_YEAR, ":", events_per_year_female$N, collapse = ", ")

att_gt_res_female <- att_gt(
    yname = "N",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR + SPECIALTY,
    data = df_model_female,
    est_method = "dr",
    control_group = "notyettreated",
    clustervars = "ID",
    pl = TRUE,
    cores = N_THREADS
)

agg_dynamic_female <- aggte(att_gt_res_female, type = "dynamic", na.rm = TRUE)
results_female <- data.frame(
    time    = agg_dynamic_female$egt,
    att     = agg_dynamic_female$att.egt,
    se      = agg_dynamic_female$se.egt
) 

# Save female long results
results_female_long <- results_female %>%
    mutate(group = "Female")
write.csv(results_female_long,
          paste0(outdir, "Supplements_Pregnancy_Female_Long_", DATE, ".csv"),
          row.names = FALSE)

data_plot <- results_female %>% filter(time >= -WIN & time <= WIN)
p_dynamic_female <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = "#1f77b4") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = expression(bold(A)~". Effect of Pregnancy on Overall Prescriptions - Female Doctors"),
        subtitle = paste0("Cases: ", n_cases_female, ", Controls: ", n_controls_female),
        x = "Years from Event",
        y = "change in total number of prescriptions"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal()

# ---------------------------------------------------------------------------
# Males

# clean marriage info
marriage_info <- fread(marriage_info_file)
marriage_info <- marriage_info %>%
    rename(
        DOCTOR_ID      = "FID",
        SPOUSE_ID      = "FID2",
        MARITAL_STATUS = "Tutkhenk_nykyinen_siviilisaaty"
    ) %>%
    mutate(
        START_DATE = as.Date(as.character(Alkupaiva), format = "%Y%m%d"),
        END_DATE   = as.Date(as.character(Paattymispaiva), format = "%Y%m%d")
    ) %>%
    mutate(
        START_YEAR = if_else(is.na(year(START_DATE)), 1998, year(START_DATE)),
        END_YEAR   = if_else(is.na(year(END_DATE)),   2023, year(END_DATE))
    ) %>%
    filter(
        DOCTOR_ID %in% doctor_ids,
        MARITAL_STATUS == 2,
        !is.na(SPOUSE_ID)
    ) %>%
    select(DOCTOR_ID, SPOUSE_ID, START_YEAR, END_YEAR)

# filter events that happened during marriage
spouse_events <- spouse_events %>%
    inner_join(marriage_info, by = "DOCTOR_ID") %>%
    filter(SPOUSE_EVENT_YEAR >= START_YEAR & SPOUSE_EVENT_YEAR <= END_YEAR)

df_model_male <- df_model %>%
    filter(SEX == "Male") %>%
    left_join(spouse_events, by = "DOCTOR_ID") %>%
    mutate(
        EVENT = if_else(!is.na(SPOUSE_EVENT_YEAR), 1, 0),
        EVENT_YEAR = if_else(!is.na(SPOUSE_EVENT_YEAR), as.numeric(SPOUSE_EVENT_YEAR), EVENT_YEAR),
        G = ifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)
    ) %>%
    select(-SPOUSE_EVENT_YEAR)

n_cases_male <- df_model_male %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls_male <- df_model_male %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()
events_per_year_male <- df_model_male[df_model_male$EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
events_year_str_male <- paste0(events_per_year_male$EVENT_YEAR, ":", events_per_year_male$N, collapse = ", ")

att_gt_res_male <- att_gt(
    yname = "N",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR + SPECIALTY,
    data = df_model_male,
    est_method = "dr",
    control_group = "notyettreated",
    clustervars = "ID",
    pl = TRUE,
    cores = N_THREADS
)

agg_dynamic_male <- aggte(att_gt_res_male, type = "dynamic", na.rm = TRUE)
results_male <- data.frame(
    time    = agg_dynamic_male$egt,
    att     = agg_dynamic_male$att.egt,
    se      = agg_dynamic_male$se.egt
) 

# Save male long results
results_male_long <- results_male %>%
    mutate(group = "Male")
write.csv(results_male_long,
          paste0(outdir, "Supplements_Pregnancy_Male_Long_", DATE, ".csv"),
          row.names = FALSE)

data_plot <- results_male %>% filter(time >= -WIN & time <= WIN)
p_dynamic_male <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = "#1f77b4") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = expression(bold(B)~". Effect of Pregnancy on Overall Prescriptions - Male Doctors (with pregnant spouses)"),
        subtitle = paste0("Cases: ", n_cases_male, ", Controls: ", n_controls_male),
        x = "Years from Event",
        y = "change in total number of prescriptions"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal()

# Join plots and save
p_combined <- p_dynamic_female + p_dynamic_male + plot_layout(ncol = 1)

ggsave(
    filename = paste0(outdir, "Plot_Supplements_Pregnancy_AB_", DATE, ".png"),
    plot = p_combined,
    width = 10, 
    height = 8, 
    dpi = 300
)

# ---------------------------------------------------------------------------
# QC: Check differences in number of events per year between groups
# ---------------------------------------------------------------------------

# Combine events data from both groups
n_events_combined <- rbind(
    events_per_year_female %>% mutate(group = "Female"),
    events_per_year_male %>% mutate(group = "Male")
)

# Calculate average N for each group
avg_n_by_group <- n_events_combined %>%
    group_by(group, EVENT_YEAR) %>%
    summarise(avg_N = mean(N, na.rm = TRUE), .groups = "drop")

# Create plot
p_n_events <- ggplot(n_events_combined, aes(x = EVENT_YEAR, y = N, color = group)) +
    geom_line() +
    geom_point(size = 3) +
    geom_line(data = avg_n_by_group, aes(x = EVENT_YEAR, y = avg_N, color = group), linetype = "dashed") +
    labs(
        title = "Number of Events by Year and Group",
        x = "Year",
        y = "Number of Events",
        color = "Group"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

ggsave(
    filename = paste0(outdir, "Plot_N_Events_By_Year_", DATE, ".png"),
    plot = p_n_events,
    width = 10, 
    height = 6, 
    dpi = 300
)


# ---------------------------------------------------------------------------
# Month zoom in for males
# ---------------------------------------------------------------------------

doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
pregnancies_file = "/media/volume/Projects/DSGELabProject1/ProcessedData/AllPregnanciesEvents_20251016.csv"
relatives_file = "/media/volume/Projects/DSGELabProject1/doctors_and_relative_20250521.csv"
# 20250926 data was extracted in months
outcomes_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Archive/Version1_Highthroughput_drop/ProcessedOutcomes_20250926/processed_outcomes.parquet"
covariates_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
outcomes = as.data.table(read_parquet(outcomes_file))
covariates = fread(covariates_file)
events = fread(pregnancies_file) 
relatives = fread(relatives_file)

# filter spouses of doctors
spouse_ids = relatives %>% filter(RELATIVE_TYPE == "SPOUSE") %>% pull(RELATIVE_ID) %>% unique()

# Filter events based on the event code
pattern <- paste0("^(", EVENT_CODE, ")")
events <- events[grepl(pattern, as.character(ICD10_CODE), perl = TRUE), .(PATIENT_ID, ICD10_CODE, VISIT_DATE)]
events$VISIT_DATE <- as.Date(events$VISIT_DATE)
events = events %>% rename(EVENT_DATE = VISIT_DATE, EVENT_CODE = ICD10_CODE)

# Generate list of doctor IDs (males) for which the spouse has an event
spouse_ids_with_event <- events %>%
    filter(PATIENT_ID %in% spouse_ids) %>%
    pull(PATIENT_ID) %>%
    unique()
pregnancy_males_ids = relatives %>% 
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    filter(RELATIVE_ID %in% spouse_ids_with_event) %>%
    pull(DOCTOR_ID) %>%
    unique()

# extract spouse event date for later use
spouse_events <- relatives %>% 
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    select(DOCTOR_ID, RELATIVE_ID) %>%
    inner_join(events %>% select(PATIENT_ID, EVENT_DATE), by = c("RELATIVE_ID" = "PATIENT_ID")) %>%
    rename(SPOUSE_EVENT_DATE = EVENT_DATE) %>%
    select(DOCTOR_ID, SPOUSE_EVENT_DATE) %>%
    distinct()

# clean marriage info
marriage_info <- fread(marriage_info_file)
marriage_info <- marriage_info %>%
    rename(
        DOCTOR_ID      = "FID",
        SPOUSE_ID      = "FID2",
        MARITAL_STATUS = "Tutkhenk_nykyinen_siviilisaaty"
    ) %>%
    mutate(
        START_DATE = as.Date(as.character(Alkupaiva), format = "%Y%m%d"),
        END_DATE   = as.Date(as.character(Paattymispaiva), format = "%Y%m%d")
    ) %>%
    mutate(
        START_DATE = if_else(is.na(START_DATE), as.Date("1998-01-01"), START_DATE),
        END_DATE   = if_else(is.na(END_DATE),   as.Date("2022-12-31"), END_DATE)
    ) %>%
    filter(
        DOCTOR_ID %in% doctor_ids,
        MARITAL_STATUS == 2,
        !is.na(SPOUSE_ID)
    ) %>%
    select(DOCTOR_ID, SPOUSE_ID, START_DATE, END_DATE)

# filter events that happened during marriage
spouse_events <- spouse_events %>%
    inner_join(marriage_info, by = "DOCTOR_ID") %>%
    filter(SPOUSE_EVENT_DATE >= START_DATE & SPOUSE_EVENT_DATE <= END_DATE) %>%
    select(DOCTOR_ID, SPOUSE_EVENT_DATE) 

events = spouse_events %>% 
    filter(DOCTOR_ID %in% pregnancy_males_ids) %>% 
    rename(DATE = SPOUSE_EVENT_DATE)
events = events[events[, .I[which.min(DATE)], by = .(DOCTOR_ID)]$V1] # only use first event

outcomes = outcomes %>% 
    filter(DOCTOR_ID %in% doctor_ids) 

# Merge events with outcomes
df_merged = left_join(outcomes, events, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
        EVENT_MONTH = if_else(!is.na(DATE), (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare covariates and specialty + merge them in the main dataframe
covariates_new = covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>% 
    mutate(SPECIALTY = if_else(SPECIALTY == "", "No specialty", SPECIALTY)) %>%
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete = merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left")
df_complete = df_complete %>% 
    mutate(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after60 = df_complete %>% filter(AGE_AT_EVENT > 60) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after60)) %>% # remove people which experiment the event after pension (age 60)
    filter(AGE <= 60) # remove all prescriptions done after pension (age 60)

df_model = df_complete %>%
    mutate(
        SPECIALTY = factor(SPECIALTY), 
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )

# Prepare the model data (monthly)
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                             
df_model$G <- ifelse(is.na(df_model$EVENT_MONTH), 0, df_model$EVENT_MONTH)        
df_model$T <- df_model$MONTH                                                      

# QC: Filter to males only 
df_model <- df_model %>% filter(SEX == "Male") 

# Count cases and controls
n_cases <- df_model %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls <- df_model %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()

# Replace/Add missing monthly N values with 0s 
df_model[is.na(N), N := 0]

# ---------------------------------------------------------------------------
# Plot case-only centered on event month

# Compute age-at-event quartile breakpoints
age_at_event_vals <- df_complete %>%
    filter(EVENT == 1) %>%
    distinct(DOCTOR_ID, AGE_AT_EVENT) %>%
    pull(AGE_AT_EVENT)

age_quartile_breaks <- quantile(age_at_event_vals, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Build quartile labels (e.g. "Q1: 28-33")
age_quartile_labels <- paste0(
    c("Q1", "Q2", "Q3", "Q4"), ": ",
    floor(age_quartile_breaks[1:4]), "-", floor(age_quartile_breaks[2:5])
)

cases_centered <- df_complete %>%
    filter(EVENT == 1) %>%
    mutate(
        event_group = case_when(
            EVENT_YEAR < 2003              ~ "Before 2003",
            EVENT_YEAR >= 2003 & EVENT_YEAR < 2013 ~ "2003-2012",
            EVENT_YEAR >= 2013 & EVENT_YEAR <= 2022 ~ "2013-2022",
            TRUE ~ NA_character_
        ),
        age_at_event_group = cut(
            AGE_AT_EVENT,
            breaks  = age_quartile_breaks,
            labels  = age_quartile_labels,
            include.lowest = TRUE
        ),
        rel_month = MONTH - EVENT_MONTH
    ) %>%
    filter(!is.na(event_group), !is.na(age_at_event_group), rel_month >= -36, rel_month <= 36)

# Per-group n cases (for subtitles)
n_cases_by_group <- cases_centered %>%
    distinct(DOCTOR_ID, event_group) %>%
    count(event_group, name = "n_cases")

cases_centered_summary <- cases_centered %>%
    group_by(event_group, age_at_event_group, rel_month) %>%
    summarise(
        mean_n = mean(N, na.rm = TRUE),
        se     = sd(N, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
    )

# Build one plot per event_group, each with its own title + subtitle (n cases)
event_groups_ordered <- c("Before 2003", "2003-2012", "2013-2022")
panel_labels <- c("A", "B", "C")

plot_list <- lapply(seq_along(event_groups_ordered), function(i) {
    grp   <- event_groups_ordered[i]
    label <- panel_labels[i]
    n_grp <- n_cases_by_group %>% filter(event_group == grp) %>% pull(n_cases)
    n_grp <- if (length(n_grp) == 0) 0L else n_grp

    dat <- cases_centered_summary %>% filter(event_group == grp)

    ggplot(dat, aes(x = rel_month, y = mean_n,
                    color = age_at_event_group, fill = age_at_event_group)) +
        geom_ribbon(aes(ymin = mean_n - 1.96 * se, ymax = mean_n + 1.96 * se),
                    alpha = 0.15, color = NA) +
        geom_line(linewidth = 1) +
        geom_point(size = 1) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        scale_x_continuous(limits = c(-36, 36), breaks = seq(-36, 36, 12)) +
        labs(
            title    = paste0(label, ". ", grp),
            subtitle = paste0("N cases: ", n_grp),
            x        = "Months from event",
            y        = "Mean total prescriptions",
            color    = "Age at event (quartile)",
            fill     = "Age at event (quartile)"
        ) +
        theme_minimal() +
        ylim(0, 150) +
        theme(
            plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey40"),
            legend.position = "bottom"
        )
})

p_cases <- wrap_plots(plot_list, ncol = 1) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

ggsave(
    filename = paste0(outdir, "Plot_Supplements_Pregnancy_Cases_Centered", DATE, ".png"),
    plot     = p_cases,
    width    = 10,
    height   = 12,
    dpi      = 300
)