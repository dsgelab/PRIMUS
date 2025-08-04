library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(patchwork)
setwd("/media/volume/Projects/mikael/ProcessedData")
source("/media/volume/Projects/mikael/utils.R")

code <- "J06.9"
code_no_dot <- gsub("\\.", "", code)
diagnosis_file <- get_latest_file(paste0("FirstConnected", code_no_dot, "Diagnoses")) # First diagnosis for each patient
prescription_file <- get_latest_file("J01Prescriptions")
doctor_file <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
patient_file <- "/media/volume/Data/Data_THL_2698_14.02.00_2023/DVV/FD_2698_Tulokset_2024-04-09_HY.csv"
city_file <- "cities.csv"
prescription_rate_file <- get_latest_file(paste0(code_no_dot, "DiagnosesWithPrescriptions"))
diag_history_pat_file <- get_latest_file(paste0(code_no_dot, "PatientDiagnosisHistory"))
pres_history_pat_file <- get_latest_file(paste0(code_no_dot, "PatientPrescriptionHistory"))
diag_history_doc_file <- get_latest_file(paste0(code_no_dot, "DoctorDiagnosisHistory"))
pres_history_doc_file <- get_latest_file(paste0(code_no_dot, "DoctorPrescriptionHistory"))

current_date <- strftime(Sys.Date(), "%Y%m%d")

# prescription <- fread(get_latest_file("/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPrescriptions_20250506.csv"))[startsWith(ATC_CODE, "J01")]
# write.csv(prescription, paste0("J01Prescriptions_", current_date, ".csv"), row.names = FALSE)
prescription <- fread(prescription_file) %>%
    as_tibble() %>%
    select(PATIENT_ID, PRESCRIPTION_DATE, DOCTOR_ID)
print(paste("Number of J01 (antibiotics) prescriptions:", nrow(prescription)))

# diagnosis <- fread(get_latest_file("AllConnectedDiagnoses"))[startsWith(ICD10_CODE, code) | startsWith(ICD10_CODE, code_no_dot)]
# # Only select the earliest instance of diagnosis for each patient
# diagnosis <- diagnosis %>%
#     as_tibble() %>%
#     arrange(PATIENT_ID, VISIT_DATE) %>%
#     group_by(PATIENT_ID) %>%
#     slice(1) %>%
#     ungroup()
# write.csv(diagnosis, paste0("FirstConnected", code_no_dot, "Diagnoses_", current_date, ".csv"), row.names = FALSE)
diagnosis <- fread(diagnosis_file) %>%
    as_tibble() %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    filter(VISIT_DATE >= min(prescription$PRESCRIPTION_DATE)) # Only include patients from the same time period as prescriptions

count <- nrow(diagnosis)
print(paste("Number of patients (first diagnoses):", count))
count_with_doctor <- nrow(diagnosis %>% filter(!is.na(DOCTOR_ID)))
percentage_with_doctor <- sprintf("%.2f%%", count_with_doctor / count * 100)
print(paste0("Number of first diagnoses connected to a doctor: ", count_with_doctor, " (", percentage_with_doctor, ")"))

codes <- unique(diagnosis$ICD10_CODE)
print(paste("All ICD10 codes starting with", code, ":", paste(codes, collapse = ", ")))

diag_history_pat <- fread(diag_history_pat_file)
pres_history_pat <- fread(pres_history_pat_file)

pres_history_doc <- fread(pres_history_doc_file)
diag_history_doc <- fread(diag_history_doc_file)

plot_theme <- theme(
    plot.title = element_text(size = 28),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 20),
    strip.text = element_text(size = 20)
)

diag_by_year_source_plot <- diagnosis %>%
    mutate(DIAGNOSIS_YEAR = format(VISIT_DATE, "%Y")) %>%
    count(DIAGNOSIS_YEAR, SOURCE) %>%
    ggplot(aes(x = DIAGNOSIS_YEAR, y = n, fill = SOURCE)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(breaks = ~ levels(factor(.x))[seq(1, nlevels(factor(.x)), by = 2)]) +
    labs(title = paste("Number of", code, "Diagnoses by Year and Source Dataset"),
         x = "Year",
         y = "Diagnoses",
         fill = "Source dataset") +
    plot_theme
diag_by_year_source_plot

diag_pres <- diagnosis %>%
    inner_join(prescription, by = "PATIENT_ID", suffix = c("_DIAG", "_PRES")) %>%
    filter(PRESCRIPTION_DATE >= VISIT_DATE) %>%
    arrange(PATIENT_ID, PRESCRIPTION_DATE) %>%
    group_by(PATIENT_ID) %>%
    slice(1) %>%
    ungroup()

# Statistics about the number of prescriptions. Same doctor means that the prescription and diagnosis were made by the same doctor.
within_week <- diag_pres %>%
    filter(as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) < 7)
same_day <- diag_pres %>%
    filter(PRESCRIPTION_DATE == VISIT_DATE)
prescription_dfs <- list(same_day = same_day, within_week = within_week)
# Prescription stats
pstats <- expand.grid(
    PRESCRIPTION_TIME = names(prescription_dfs),
    DOCTOR_MATCH = c("same", "different", "missing")
)
for (prescription_time in names(prescription_dfs)) {
    df = prescription_dfs[[prescription_time]]
    pstats[pstats$PRESCRIPTION_TIME == prescription_time & pstats$DOCTOR_MATCH == "same", "COUNT"] = nrow(df %>% filter(DOCTOR_ID_DIAG == DOCTOR_ID_PRES))
    pstats[pstats$PRESCRIPTION_TIME == prescription_time & pstats$DOCTOR_MATCH == "different", "COUNT"] = nrow(df %>% filter(DOCTOR_ID_DIAG != DOCTOR_ID_PRES))
    pstats[pstats$PRESCRIPTION_TIME == prescription_time & pstats$DOCTOR_MATCH == "missing", "COUNT"] = nrow(df %>% filter(is.na(DOCTOR_ID_DIAG)))
}
stopifnot(nrow(within_week) == sum(pstats[pstats$PRESCRIPTION_TIME == "within_week", "COUNT"]))
stopifnot(nrow(same_day) == sum(pstats[pstats$PRESCRIPTION_TIME == "same_day", "COUNT"]))

pres_timing_plot <- ggplot(pstats, aes(x = DOCTOR_MATCH, y = COUNT, fill = PRESCRIPTION_TIME)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(
        title = paste(code, "Prescription Timing by Doctor Match"),
        x = "Doctor Match",
        y = "Count",
        fill = "Prescription Time"
    ) +
    plot_theme
pres_timing_plot

city <- fread(city_file) %>% as_tibble()

patient <- fread(patient_file) %>%
    as_tibble() %>%
    rename(BIRTH_DATE = "Syntymä-päivä", SEX = "Suku-.puoli", PATIENT_ID = FID) %>%
    mutate(BIRTH_DATE = ymd(BIRTH_DATE)) %>%
    rename(CITY = "Kotikunnan.nimi") %>%
    left_join(city, by = "CITY") %>%
    rename(HOME_REGION = REGION) %>%
    mutate(BIRTH_YEAR = year(BIRTH_DATE)) %>%
    select(PATIENT_ID, BIRTH_YEAR, BIRTH_DATE, SEX, HOME_REGION)

doctor <- fread(doctor_file) %>%
    as_tibble() %>%
    rename(SPECIALTY = INTERPRETATION) %>%
    mutate(SPECIALTY = replace(SPECIALTY, SPECIALTY == "" | is.na(SPECIALTY), "No Specialty")) %>%
    rename(LANGUAGE_DOC = LANGUAGE) %>%
    select(DOCTOR_ID, PRACTICING_DAYS, SPECIALTY, LANGUAGE_DOC)


# Summarizes all diagnoses and whether a prescription was made after the diagnosis. The prescription information is
# imputed by assigning a prescription to a patient who received an antibiotic prescription from the same doctor as the
# diagnosis on the same day. Prescriptions within a week from any doctor are classified as "unclear" (excluding same-doctor
# same-day prescriptions).
prescription_rate_init <- diagnosis %>%
  left_join(prescription, by = "PATIENT_ID", suffix = c("_DIAG", "_PRES")) %>%
  # Indicator for prescriptions and unclear prescriptions
  mutate(
    UNCLEAR_OR_PRES = !is.na(PRESCRIPTION_DATE) & 
      (as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) < 7 & 
       as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) >= 0)
  ) %>%
  # Select only one row per patient, preferably the most recent prescription (if any)
  arrange(PATIENT_ID, desc(UNCLEAR_OR_PRES), PRESCRIPTION_DATE) %>%
  group_by(PATIENT_ID) %>%
  slice(1) %>%
  ungroup()

pr <- prescription_rate_init
prescribed_condition <- pr$VISIT_DATE == pr$PRESCRIPTION_DATE & (is.na(pr$DOCTOR_ID_DIAG) | pr$DOCTOR_ID_DIAG == pr$DOCTOR_ID_PRES)
prescribed_condition_strict <- pr$VISIT_DATE == pr$PRESCRIPTION_DATE & pr$DOCTOR_ID_DIAG == pr$DOCTOR_ID_PRES
n_prescribed <- prescription_rate_init %>% filter(prescribed_condition_strict) %>% nrow()
n_not_prescribed <- prescription_rate_init %>%
    filter(
        is.na(PRESCRIPTION_DATE) |
            as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) >= 7 |
            PRESCRIPTION_DATE < VISIT_DATE
    ) %>%
    nrow()
# Get the number of unclear prescriptions with formula sum(I(unclear or prescribed) - I(prescribed))
n_unclear <- prescription_rate_init %>% filter(UNCLEAR_OR_PRES == 1) %>% nrow() - n_prescribed
stopifnot(n_prescribed + n_not_prescribed + n_unclear == nrow(prescription_rate_init))
prescription_classes <- tibble(
    CLASS = c("Prescribed", "Not Prescribed", "Unclear"),
    COUNT = c(n_prescribed, n_not_prescribed, n_unclear),
) %>%
    mutate(PERCENTAGE = COUNT / sum(COUNT) * 100)
prescription_classes

pres_class_plot <- ggplot(prescription_classes, aes(x = CLASS, y = COUNT, fill = CLASS)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", PERCENTAGE)), 
              vjust = -0.3, size = 6) +
    labs(
        title = paste(code, "Patients with Prescription vs No Prescription vs Unclear status"),
        y = "Frequency",
        x = NULL,
        fill = "Label"
    ) +
    plot_theme
pres_class_plot

calc_age <- function(birth_date, current_date) {
    as.numeric(difftime(current_date, birth_date, units = "days") / 365.25)
}

# Filter out unclear prescriptions from further analysis. Impute diagnosing doctor from prescribing doctor. Add
# doctor and patient characteristics.
prescription_rate <- prescription_rate_init %>%
    filter(prescribed_condition_strict | UNCLEAR_OR_PRES == 0) %>%
    rename(PRESCRIBED = UNCLEAR_OR_PRES) %>%
    mutate(PRESCRIBED = as.numeric(PRESCRIBED)) %>%
    #mutate(DOCTOR_ID = ifelse(PRESCRIBED == 1, DOCTOR_ID_PRES, DOCTOR_ID_DIAG)) %>%  # Use if unknown diagnosing doctors are imputed from prescribing doctors
    mutate(DOCTOR_ID = DOCTOR_ID_DIAG) %>% # Use if unknown diagnosing doctors are not imputed from prescribing doctors
    left_join(doctor, by = "DOCTOR_ID") %>%
    left_join(patient, by = c("DOCTOR_ID" = "PATIENT_ID")) %>%  # Doctors are also patients
    left_join(patient, by = "PATIENT_ID", suffix = c("_DOC", "_PAT")) %>%
    mutate(AGE_AT_VISIT_DOC = calc_age(BIRTH_DATE_DOC, VISIT_DATE)) %>%
    mutate(AGE_AT_VISIT_PAT = calc_age(BIRTH_DATE_PAT, VISIT_DATE)) %>%
    inner_join(diag_history_pat, by = "PATIENT_ID") %>%
    inner_join(pres_history_pat, by = "PATIENT_ID") %>%
    left_join(diag_history_doc, by = c("DOCTOR_ID", "VISIT_DATE"), suffix = c("_PAT", "_DOC")) %>%
    left_join(pres_history_doc, by = c("DOCTOR_ID", "VISIT_DATE"), suffix = c("_PAT", "_DOC"))
# write.csv(prescription_rate, paste0(code_no_dot, "DiagnosesWithPrescriptions_", current_date, ".csv"), row.names = FALSE)
# prescription_rate <- fread(prescription_rate_file) %>% as_tibble

# Class imbalance plot
class_freq <- tibble(
    CLASS = c("Prescribed", "Not Prescribed"),
    COUNT = c(sum(prescription_rate$PRESCRIBED), sum(prescription_rate$PRESCRIBED == 0))
) %>%
    mutate(PERCENTAGE = COUNT / sum(COUNT) * 100)

class_freq_plot <- ggplot(class_freq, aes(x = CLASS, y = COUNT, fill = CLASS)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.1f%%", PERCENTAGE)), 
              vjust = -0.3, size = 6) +
    labs(
        title = paste(code, "Patients with Prescription vs No Prescription"),
        y = "Frequency",
        fill = "Label",
        x = NULL
    ) +
    plot_theme
class_freq_plot

# Number of prescriptions per year
yearly_prescriptions <- prescription_rate %>%
    filter(PRESCRIBED == 1) %>%
    mutate(YEAR = lubridate::year(VISIT_DATE)) %>%
    group_by(YEAR) %>%
    summarize(PRESCRIPTION_COUNT = n())

yearly_pres_plot <- ggplot(yearly_prescriptions, aes(x = factor(YEAR), y = PRESCRIPTION_COUNT)) +
    geom_bar(stat = "identity") +
    labs(
        title = paste("Number of", code, "Prescriptions per Year"),
        x = "Year",
        y = "Prescription Count"
    ) +
    plot_theme
yearly_pres_plot

visit_year_counts <- prescription_rate %>%
    mutate(VISIT_YEAR = lubridate::year(VISIT_DATE)) %>%
    count(VISIT_YEAR, PRESCRIBED)
unique_visit_years <- sort(unique(visit_year_counts$VISIT_YEAR))
n_visit_years <- length(unique_visit_years)
get_year_breaks <- function(years, n_years) if (n_years > 12) years[seq(1, n_years, by = 2)] else years
diag_by_year_pres_plot <- ggplot(visit_year_counts, aes(x = factor(VISIT_YEAR), y = n, fill = factor(PRESCRIBED))) +
    geom_bar(stat = "identity") +
    labs(
        title = paste("Number of", code, "Diagnoses by Year and Prescription Status"),
        x = "Year",
        y = "Diagnoses",
        fill = "Prescribed"
    ) +
    scale_fill_manual(
        values = c("0" = "#F8766D", "1" = "#00BFC4"),
        labels = c("0" = "No", "1" = "Yes")
    ) +
    scale_x_discrete(breaks = get_year_breaks(unique_visit_years, n_visit_years)) +
    plot_theme
diag_by_year_pres_plot

# Distribution of diagnosing and prescribing doctors by specialty
diag_freq_by_specialty <- prescription_rate %>%
    filter(!is.na(SPECIALTY)) %>%
    group_by(SPECIALTY) %>%
    summarize(
        DIAGNOSIS_FREQ = n(),
    ) %>%
    mutate(DIAGNOSIS_FREQ = DIAGNOSIS_FREQ / sum(DIAGNOSIS_FREQ) * 100)
pres_freq_by_specialty <- prescription_rate %>%
    filter(!is.na(SPECIALTY)) %>%
    filter(PRESCRIBED == 1) %>%
    group_by(SPECIALTY) %>%
    summarize(
        PRESCRIPTION_FREQ = n(),
    ) %>%
    mutate(PRESCRIPTION_FREQ = PRESCRIPTION_FREQ / sum(PRESCRIPTION_FREQ) * 100)
freq_by_specialty <- diag_freq_by_specialty %>%
    inner_join(pres_freq_by_specialty, by = "SPECIALTY") %>%
    # Filter out specialties with frequency lower than 0.5%
    filter(DIAGNOSIS_FREQ > 0.5 | PRESCRIPTION_FREQ > 0.5) %>%
    # Convert to long format. That is, one row for each frequency.
    pivot_longer(
        cols = c(DIAGNOSIS_FREQ, PRESCRIPTION_FREQ),
        names_to = "FREQUENCY_TYPE",
        values_to = "FREQUENCY"
    ) %>%
    mutate(FREQUENCY_TYPE = gsub("_FREQ", "", FREQUENCY_TYPE)) %>%
    arrange(desc(FREQUENCY))

diagnosis_sum <- sum(freq_by_specialty %>% filter(FREQUENCY_TYPE == "DIAGNOSIS") %>% pull(FREQUENCY))
prescription_sum <- sum(freq_by_specialty %>% filter(FREQUENCY_TYPE == "PRESCRIPTION") %>% pull(FREQUENCY))
stopifnot(diagnosis_sum > 95 & diagnosis_sum < 100)
stopifnot(prescription_sum > 95 & prescription_sum < 100)

freq_by_specialty_plot <- ggplot(freq_by_specialty, aes(x = reorder(SPECIALTY, FREQUENCY), y = FREQUENCY, fill = FREQUENCY_TYPE)) +
    geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
    coord_flip() +
    labs(
        title = paste("Distribution of", code, "Prescriptions and\nDiagnoses by Specialty"),
        x = "Specialty",
        y = "Relative Frequency (%)",
        fill = "Frequency Type"
    ) +
    scale_fill_discrete(
        labels = c("DIAGNOSIS" = "Diagnosis", "PRESCRIPTION" = "Prescription")
    ) +
    plot_theme
freq_by_specialty_plot

add_binom_interval <- function(df, count_col, n_col, conf_level = 0.95) {
  z <- qnorm((1 + conf_level) / 2)
  df %>%
    mutate(
      p = .data[[count_col]] / .data[[n_col]],
      margin = z * sqrt(p * (1 - p) / .data[[n_col]]),
      LOWER_BOUND = pmax(p - margin, 0) * 100,
      UPPER_BOUND = pmin(p + margin, 1) * 100,
    ) %>%
    select(-p, -margin)  # Remove temporary columns
}

rate_by_specialty <- prescription_rate %>%
    filter(!is.na(SPECIALTY)) %>%
    group_by(SPECIALTY) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    mutate(RELATIVE_FREQ = TOTAL / sum(TOTAL) * 100) %>%
    # Filter out specialties with relative frequency lower than 0.5%
    filter(RELATIVE_FREQ > 0.5) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL") %>%
    arrange(-TOTAL)
mean_prescription_rate <- mean(prescription_rate %>% filter(!is.na(SPECIALTY)) %>% pull(PRESCRIBED)) * 100

rate_by_specialty_plot <- ggplot(rate_by_specialty, aes(x = reorder(SPECIALTY, PRESCRIBED_RATE), y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    geom_hline(
        yintercept = mean_prescription_rate,
        linetype = "dashed",
        color = "red"
    ) +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    annotate("text",
        x = 0.3,
        y = mean_prescription_rate,
        label = "Mean Prescription Rate",
        color = "red",
        size = 7
    ) +
    scale_x_discrete(
        expand = expansion(mult = c(0.1, 0))
    ) +
    labs(
        title = "J06.9 Prescription Rate by Specialty",
        x = "Specialty",
        y = "Prescription Rate (%)"
    ) +
    plot_theme
rate_by_specialty_plot

yearly_rate <- prescription_rate %>%
    mutate(YEAR = lubridate::year(VISIT_DATE)) %>%
    group_by(YEAR) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100)

yearly_rate_plot <- ggplot(yearly_rate, aes(x = YEAR, y = PRESCRIBED_RATE)) +
    geom_line() +
    labs(
        title = "J06.9 Prescription Rate over Time",
        x = "Year",
        y = "Prescription Rate (%)"
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    plot_theme
yearly_rate_plot

# Histogram of ages
age_doc_pres_plot <- ggplot(prescription_rate %>% filter(!is.na(AGE_AT_VISIT_DOC)), aes(x = AGE_AT_VISIT_DOC, fill = factor(PRESCRIBED))) +
    geom_density(
        alpha = 0.4,
    ) +
    scale_fill_manual(
        values = c("0" = "#f4c0bd", "1" = "#91dddf"),
        labels = c("0" = "Did not prescribe", "1" = "Prescribed")
    ) +
    labs(
        title = "Age Distribution of Doctors Prescribing and not Prescribing to J06.9 Patients",
        x = "Age",
        y = "Probability density",
        fill = "Prescribed"
    ) +
    plot_theme
age_doc_pres_plot

rate_by_age_doc <- prescription_rate %>%
    filter(AGE_AT_VISIT_DOC < 90 & AGE_AT_VISIT_DOC > 20) %>%
    # Discretize doctor age to bins of 5 years
    mutate(
        AGE_BIN_DOC = cut(
            AGE_AT_VISIT_DOC,
            breaks = seq(from = floor(min(AGE_AT_VISIT_DOC) / 5) * 5, to = ceiling(max(AGE_AT_VISIT_DOC) / 5) * 5, by = 5),
            labels = paste0(seq(floor(min(AGE_AT_VISIT_DOC) / 5) * 5, ceiling(max(AGE_AT_VISIT_DOC) / 5) * 5 - 5, by = 5), "-",
                            seq(floor(min(AGE_AT_VISIT_DOC) / 5) * 5 + 4, ceiling(max(AGE_AT_VISIT_DOC) / 5) * 5 - 1, by = 5)),
            right = FALSE
        )
    ) %>%
    group_by(AGE_BIN_DOC) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

# Prescription rate by doctor age
rate_by_age_doc_plot <- ggplot(rate_by_age_doc, aes(x = AGE_BIN_DOC, y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    labs(
        title = "J06.9 Prescription Rate by Doctor Age",
        x = "Age",
        y = "Prescription Rate (%)"
    ) +
    plot_theme
rate_by_age_doc_plot

# Prescription rate by doctor sex
rate_by_sex_doc <- prescription_rate %>%
    filter(!is.na(SEX_DOC)) %>%
    group_by(SEX_DOC) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

rate_by_sex_doc_plot <- ggplot(rate_by_sex_doc, aes(x = factor(SEX_DOC), y = PRESCRIBED_RATE, fill = factor(SEX_DOC))) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    scale_fill_manual(
        values = c("1" = "#f8766d", "2" = "#619cff"),
        labels = c("1" = "Male", "2" = "Female")
    ) +
    labs(
        title = "J06.9 Prescription Rate by Doctor Sex",
        fill = "Sex",
        x = "Sex",
        y = "Prescription Rate (%)"
    ) +
    plot_theme +
    theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
    )
rate_by_sex_doc_plot

# Prescription rate by month
monthly_rate <- prescription_rate %>%
    mutate(MONTH = lubridate::month(VISIT_DATE)) %>%
    group_by(MONTH) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(
        PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100
    ) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL") %>%
    arrange(-TOTAL)

monthly_rate_plot <- ggplot(monthly_rate, aes(x = factor(MONTH), y = PRESCRIBED_RATE)) + 
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    labs(
        title = "J06.9 Prescription Rate by Month",
        x = "Month",
        y = "Prescription Rate (%)"
    ) +
    plot_theme
monthly_rate_plot

rate_by_weekday <- prescription_rate %>%
    mutate(WEEKDAY = lubridate::wday(VISIT_DATE, week_start = 1)) %>%
    group_by(WEEKDAY) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

rate_by_weekday_plot <- ggplot(rate_by_weekday, aes(x = factor(WEEKDAY), y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    labs(
        title = "J06.9 Prescription Rate by Weekday",
        x = "Weekday",
        y = "Prescription Rate (%)"
    ) +
    plot_theme
rate_by_weekday_plot

# Prescription rate by J06.9 subcode
rate_by_code <- prescription_rate %>%
    filter(VISIT_DATE >= min(prescription$PRESCRIPTION_DATE)) %>%
    group_by(ICD10_CODE) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL") %>%
    arrange(-TOTAL)

subcode_freq_plot <- ggplot(rate_by_code, aes(x = reorder(ICD10_CODE, TOTAL), y = TOTAL)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
        title = "Distribution of J06.9 Subcodes",
        x = "Subcode",
        y = "Frequency"
    ) +
    plot_theme
subcode_freq_plot

rate_by_code_plot <- ggplot(rate_by_code %>% filter(TOTAL > 1000), aes(x = reorder(ICD10_CODE, TOTAL), y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    labs(
        title = "J06.9 Prescription Rate by Subcode",
        x = "Subcode",
        y = "Prescription Rate (%)"
    ) +
    plot_theme
rate_by_code_plot

# Prescription rate by source dataset
rate_by_source <- prescription_rate %>%
    group_by(SOURCE) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

rate_by_source_plot <- ggplot(rate_by_source, aes(x = factor(SOURCE), y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    labs(
        title = "J06.9 Prescription Rate by Source Dataset",
        x = "Source Dataset",
        y = "Prescription Rate (%)"
    ) +
    plot_theme
rate_by_source_plot

# Prescription rate by doctor and patient home region
rate_by_doc_region <- prescription_rate %>%
    filter(!is.na(HOME_REGION_DOC)) %>%
    group_by(HOME_REGION_DOC) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")
rate_by_pat_region <- prescription_rate %>%
    filter(!is.na(HOME_REGION_PAT)) %>%
    group_by(HOME_REGION_PAT) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")
rate_by_region <- rate_by_doc_region %>%
    inner_join(rate_by_pat_region, by = c("HOME_REGION_DOC" = "HOME_REGION_PAT"), suffix = c("_DOCTOR", "_PATIENT")) %>%
    rename(REGION = HOME_REGION_DOC) %>%
    pivot_longer(
        cols = -REGION,
        names_to = c("measure", "PERSON_TYPE"),
        names_sep = "_(?=[^_]+$)",
        values_to = "value"
    ) %>%
    pivot_wider(
        names_from = measure,
        values_from = value
    ) %>%
    arrange(desc(PRESCRIBED_RATE))

rate_by_region_plot <- ggplot(rate_by_region, aes(x = reorder(REGION, PRESCRIBED_RATE), y = PRESCRIBED_RATE, fill = PERSON_TYPE)) +
    geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
    coord_flip() +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2, position = position_dodge2(width = 0.9, reverse = TRUE)) +
    labs(
        title = "Prescription Rate by Doctor and Patient Home Region",
        x = "Region",
        y = "Prescription rate (%)",
        fill = "Person Type"
    ) +
    scale_fill_discrete(
        labels = c("DOCTOR" = "Doctor home region", "PATIENT" = "Patient home region")
    ) +
    plot_theme
rate_by_region_plot

rate_by_language <- prescription_rate %>%
    filter(!is.na(LANGUAGE_DOC)) %>%
    group_by(LANGUAGE_DOC) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL") %>%
    arrange(-TOTAL)

# Distribution of doctor languages
lang_freq_plot <- ggplot(rate_by_language %>% filter(TOTAL > 1000), aes(x = reorder(LANGUAGE_DOC, TOTAL), y = TOTAL)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
        title = "Distribution of Doctor Languages",
        x = "Language",
        y = "Frequency"
    ) +
    plot_theme
lang_freq_plot

# Prescription rate by doctor language
rate_by_lang_plot <- ggplot(rate_by_language %>% filter(TOTAL > 10000), aes(x = reorder(LANGUAGE_DOC, PRESCRIBED_RATE), y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    coord_flip() +
    labs(
        title = "Prescription Rate by Doctor Language",
        x = "Language",
        y = "Prescription Rate (%)"
    ) +
    plot_theme
rate_by_lang_plot

# Histogram of patient ages
age_pat_freq_plot <- ggplot(prescription_rate %>% filter(!is.na(AGE_AT_VISIT_PAT)), aes(x = AGE_AT_VISIT_PAT)) +
    geom_histogram(binwidth = 2) +
    labs(
        title = "Age Distribution of Patients Diagnosed with J06.9",
        x = "Age",
        y = "Frequency",
    ) +
    scale_x_continuous(
        n.breaks = 10
    ) +
    plot_theme
age_pat_freq_plot

# Distribution of patient sex
pat_sex_freq_plot <- prescription_rate %>% 
  filter(!is.na(SEX_PAT)) %>% 
  count(SEX_PAT) %>% 
  mutate(PERCENTAGE = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = PERCENTAGE, fill = factor(SEX_PAT))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(PERCENTAGE), "%")),
           position = position_stack(vjust = 0.5),
           size = 10) +
  labs(title = "Sex Distribution of Patients Diagnosed with J06.9",
       fill = "Sex",
       x = NULL,
       y = NULL) +
  scale_fill_manual(
    values = c("1" = "#f8766d", "2" = "#619cff"),
    labels = c("1" = "Male", "2" = "Female")
  ) +
  theme_void() +
  plot_theme +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )
pat_sex_freq_plot

# Histogram of doctor ages
age_doc_freq_plot <- ggplot(prescription_rate %>% filter(!is.na(AGE_AT_VISIT_DOC)), aes(x = AGE_AT_VISIT_DOC)) +
    geom_histogram(binwidth = 2) +
    labs(
        title = "Age Distribution of Doctors Diagnosing J06.9",
        x = "Age",
        y = "Frequency",
    ) +
    scale_x_continuous(
        n.breaks = 10
    ) +
    plot_theme
age_doc_freq_plot

# Distribution of doctor sex
doc_sex_freq <- prescription_rate %>%
    filter(!is.na(SEX_DOC)) %>%
    group_by(SEX_DOC) %>%
    summarize(
        COUNT = n_distinct(DOCTOR_ID)
    ) %>%
    mutate(PERCENTAGE = COUNT / sum(COUNT) * 100)

doc_sex_freq_plot <- ggplot(doc_sex_freq, aes(x = "", y = PERCENTAGE, fill = factor(SEX_DOC))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(PERCENTAGE), "%")),
            position = position_stack(vjust = 0.5),
            size = 10) +
    labs(title = "Sex Distribution of Doctors Diagnosing J06.9",
        fill = "Sex",
        x = NULL,
        y = NULL) +
    scale_fill_manual(
    values = c("1" = "#f8766d", "2" = "#619cff"),
    labels = c("1" = "Male", "2" = "Female")
    ) +
    theme_void() +
    plot_theme +
    theme(
    axis.ticks = element_blank(),
    axis.text = element_blank()
    )
doc_sex_freq_plot

# Histogram of patient ages given prescription status
age_pat_pres_plot <- ggplot(prescription_rate %>% filter(!is.na(AGE_AT_VISIT_PAT)), aes(x = AGE_AT_VISIT_PAT, fill = factor(PRESCRIBED))) +
    geom_density(
        alpha = 0.4,
    ) +
    scale_fill_manual(
        values = c("0" = "#f4c0bd", "1" = "#91dddf"),
        labels = c("0" = "Did not prescribe", "1" = "Prescribed")
    ) +
    labs(
        title = "Age Distribution of J06.9 Patients Being Prescribed and not Prescribed Antibiotics",
        x = "Age",
        y = "Probability density",
        fill = "Prescribed"
    ) +
    plot_theme
age_pat_pres_plot

# Prescription rate by patient age
rate_by_age_pat <- prescription_rate %>%
    filter(!is.na(AGE_AT_VISIT_PAT) & AGE_AT_VISIT_PAT < 100) %>%
    mutate(
        AGE_BIN_PAT = cut(
            AGE_AT_VISIT_PAT,
            breaks = seq(from = floor(min(AGE_AT_VISIT_PAT) / 5) * 5, to = ceiling(max(AGE_AT_VISIT_PAT) / 5) * 5, by = 5),
            labels = paste0(seq(floor(min(AGE_AT_VISIT_PAT) / 5) * 5, ceiling(max(AGE_AT_VISIT_PAT) / 5) * 5 - 5, by = 5), "-",
                            seq(floor(min(AGE_AT_VISIT_PAT) / 5) * 5 + 4, ceiling(max(AGE_AT_VISIT_PAT) / 5) * 5 - 1, by = 5)),
            right = FALSE
        )
    ) %>%
    group_by(AGE_BIN_PAT) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

age_bin_labels <- as.character(rate_by_age_pat$AGE_BIN_PAT)
age_bin_labels[seq(2, length(age_bin_labels), by = 2)] <- ""
rate_by_age_pat_plot <- ggplot(rate_by_age_pat, aes(x = AGE_BIN_PAT, y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    labs(
        title = "J06.9 Prescription Rate by Patient Age",
        x = "Age",
        y = "Prescription Rate (%)"
    ) +
    scale_x_discrete(labels = age_bin_labels) +
    plot_theme
rate_by_age_pat_plot

# Prescription rate by patient sex
rate_by_sex_pat <- prescription_rate %>%
    filter(!is.na(SEX_PAT)) %>%
    group_by(SEX_PAT) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

rate_by_sex_pat_plot <- ggplot(rate_by_sex_pat, aes(x = factor(SEX_PAT), y = PRESCRIBED_RATE, fill = factor(SEX_PAT))) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    scale_fill_manual(
        values = c("1" = "#f8766d", "2" = "#619cff"),
        labels = c("1" = "Male", "2" = "Female")
    ) +
    labs(
        title = "J06.9 Prescription Rate by Patient Sex",
        fill = "Sex",
        x = "Sex",
        y = "Prescription Rate (%)"
    ) +
    plot_theme +
    theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
    )
rate_by_sex_pat_plot

# Prevalence of different diseases for patients with J06.9
patient_disease_pattern <- "^HAD_ICD10_(.+)_PAT$"
disease_prevalence_pat <- prescription_rate %>%
    summarize(across(matches(patient_disease_pattern), ~ mean(.x) * 100)) %>%
    pivot_longer(cols = matches(patient_disease_pattern), names_to = "DISEASE_HISTORY", values_to = "PREVALENCE") %>%
    mutate(DISEASE_HISTORY = sub(patient_disease_pattern, "\\1", DISEASE_HISTORY))

disease_prevalence_pat_plot <- ggplot(disease_prevalence_pat, aes(x = DISEASE_HISTORY, y = PREVALENCE)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Prevalence of Different Diseases for Patients with J06.9",
        x = "ICD10 Code First Character",
        y = "Prevalence (%)"
    ) +
    plot_theme
disease_prevalence_pat_plot

rate_by_diag_history_pat <- prescription_rate %>%
    pivot_longer(cols = matches(patient_disease_pattern),
                names_to = "DISEASE_INDICATOR",
                values_to = "INDICATOR_VALUE") %>%
    group_by(DISEASE_INDICATOR, INDICATOR_VALUE) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n(),
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    mutate(DISEASE_INDICATOR = sub(patient_disease_pattern, "\\1", DISEASE_INDICATOR)) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

rate_by_diag_history_pat_plot <-
    ggplot(rate_by_diag_history_pat, aes(x = reorder(DISEASE_INDICATOR, PRESCRIBED_RATE), y = PRESCRIBED_RATE, fill = factor(INDICATOR_VALUE))) +
    geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
    coord_flip() +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2, position = position_dodge2(width = 0.9, reverse = TRUE)) +
    labs(
        title = "Prescription Rate by Patient Disease History",
        x = "ICD10 Code First Character",
        y = "Prescription rate (%)",
        fill = "Whether Has Diagnosis"
    ) +
    scale_fill_discrete(
        labels = c("0" = "Has no diagnosis", "1" = "Has diagnosis")
    ) +
    plot_theme
rate_by_diag_history_pat_plot

# Prevalence of different prescriptions for patients with J06.9
patient_prescription_pattern <- "^GOT_ATC_(.+)_PAT$"
prescription_prevalence_pat <- prescription_rate %>%
    summarize(across(matches(patient_prescription_pattern), ~ mean(.x) * 100)) %>%
    pivot_longer(cols = matches(patient_prescription_pattern), names_to = "PRESCRIPTION_HISTORY", values_to = "PREVALENCE") %>%
    mutate(PRESCRIPTION_HISTORY = sub(patient_prescription_pattern, "\\1", PRESCRIPTION_HISTORY))

prescription_prevalence_pat_plot <- ggplot(prescription_prevalence_pat, aes(x = PRESCRIPTION_HISTORY, y = PREVALENCE)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Prevalence of Different Prescriptions for Patients with J06.9",
        x = "ATC Code First Character",
        y = "Prevalence"
    ) +
    plot_theme
prescription_prevalence_pat_plot

rate_by_pres_history_pat <- prescription_rate %>%
    pivot_longer(cols = matches(patient_prescription_pattern),
                names_to = "MEDICATION_INDICATOR",
                values_to = "INDICATOR_VALUE") %>%
    group_by(MEDICATION_INDICATOR, INDICATOR_VALUE) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n(),
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    mutate(MEDICATION_INDICATOR = sub(patient_prescription_pattern, "\\1", MEDICATION_INDICATOR)) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")
 
rate_by_pres_history_pat_plot <- ggplot(rate_by_pres_history_pat, aes(x = reorder(MEDICATION_INDICATOR, PRESCRIBED_RATE), y = PRESCRIBED_RATE, fill = factor(INDICATOR_VALUE))) +
    geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
    coord_flip() +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2, position = position_dodge2(width = 0.9, reverse = TRUE)) +
    labs(
        title = "Prescription Rate by Patient Medication History",
        x = "ATC Code First Character",
        y = "Prescription rate (%)",
        fill = "Has Used Medication"
    ) +
    scale_fill_discrete(
        labels = c("0" = "Not Used", "1" = "Used")
    ) +
    plot_theme
rate_by_pres_history_pat_plot

# Prevalence of different diseases for doctors with J06.9

doctor_disease_pattern <- "^HAD_ICD10_(.+)_DOC$"
disease_prevalence_doc <- prescription_rate %>%
    filter(!is.na(HAD_ICD10_A_DOC)) %>%
    summarize(across(matches(doctor_disease_pattern), ~ mean(.x) * 100)) %>%
    pivot_longer(cols = matches(doctor_disease_pattern), names_to = "DISEASE_HISTORY", values_to = "PREVALENCE") %>%
    mutate(DISEASE_HISTORY = sub(doctor_disease_pattern, "\\1", DISEASE_HISTORY))

disease_prevalence_doc_plot <- ggplot(disease_prevalence_doc, aes(x = DISEASE_HISTORY, y = PREVALENCE)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Prevalence of Different Diseases for Doctors Diagnosing J06.9",
        x = "ICD10 Code First Character",
        y = "Prevalence (%)"
    ) +
    plot_theme
disease_prevalence_doc_plot

rate_by_diag_history_doc <- prescription_rate %>%
    filter(!is.na(HAD_ICD10_A_DOC)) %>%
    pivot_longer(cols = matches(doctor_disease_pattern),
                names_to = "DISEASE_INDICATOR",
                values_to = "INDICATOR_VALUE") %>%
    group_by(DISEASE_INDICATOR, INDICATOR_VALUE) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n(),
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    mutate(DISEASE_INDICATOR = sub(doctor_disease_pattern, "\\1", DISEASE_INDICATOR)) %>%
    # Remove both rows for a disease if either INDICATOR_VALUE (0 or 1) has TOTAL < 1000
    group_by(DISEASE_INDICATOR) %>%
    filter(all(TOTAL >= 1000)) %>%
    ungroup() %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

rate_by_diag_history_doc_plot <- ggplot(rate_by_diag_history_doc, aes(x = reorder(DISEASE_INDICATOR, PRESCRIBED_RATE), y = PRESCRIBED_RATE, fill = factor(INDICATOR_VALUE))) +
    geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
    coord_flip() +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2, position = position_dodge2(width = 0.9, reverse = TRUE)) +
    labs(
        title = "Prescription Rate by Doctor Disease History",
        x = "ICD10 Code First Character",
        y = "Prescription rate (%)",
        fill = "Whether Has Diagnosis"
    ) +
    scale_fill_discrete(
        labels = c("0" = "Has no diagnosis", "1" = "Has diagnosis")
    ) +
    plot_theme
rate_by_diag_history_doc_plot

# Prevalence of different prescriptions for doctors with J06.9
doctor_prescription_pattern <- "^GOT_ATC_(.+)_DOC$"
prescription_prevalence_doc <- prescription_rate %>%
    filter(!is.na(GOT_ATC_A_DOC)) %>%
    summarize(across(matches(doctor_prescription_pattern), ~ mean(.x) * 100)) %>%
    pivot_longer(cols = matches(doctor_prescription_pattern), names_to = "PRESCRIPTION_HISTORY", values_to = "PREVALENCE") %>%
    mutate(PRESCRIPTION_HISTORY = sub(doctor_prescription_pattern, "\\1", PRESCRIPTION_HISTORY))

prescription_prevalence_doc_plot <- ggplot(prescription_prevalence_doc, aes(x = PRESCRIPTION_HISTORY, y = PREVALENCE)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Prevalence of Different Prescriptions for Doctors Diagnosing J06.9",
        x = "ATC Code First Character",
        y = "Prevalence"
    ) +
    plot_theme
prescription_prevalence_doc_plot

rate_by_pres_history_doc <- prescription_rate %>%
    filter(!is.na(GOT_ATC_A_DOC)) %>%
    pivot_longer(cols = matches(doctor_prescription_pattern),
                names_to = "MEDICATION_INDICATOR",
                values_to = "INDICATOR_VALUE") %>%
    group_by(MEDICATION_INDICATOR, INDICATOR_VALUE) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n(),
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    mutate(MEDICATION_INDICATOR = sub(doctor_prescription_pattern, "\\1", MEDICATION_INDICATOR)) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")
 
rate_by_pres_history_doc_plot <- ggplot(rate_by_pres_history_doc, aes(x = reorder(MEDICATION_INDICATOR, PRESCRIBED_RATE), y = PRESCRIBED_RATE, fill = factor(INDICATOR_VALUE))) +
    geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
    coord_flip() +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2, position = position_dodge2(width = 0.9, reverse = TRUE)) +
    labs(
        title = "Prescription Rate by Doctor Medication History",
        x = "ATC Code First Character",
        y = "Prescription rate (%)",
        fill = "Has Used Medication"
    ) +
    scale_fill_discrete(
        labels = c("0" = "Not Used", "1" = "Used")
    ) +
    plot_theme
rate_by_pres_history_doc_plot

# Histogram of mean yearly past prescriptions
mean_past_pres_hist <- ggplot(prescription_rate %>% filter(!is.na(MEAN_YEARLY_PRESCRIPTIONS)), aes(x = MEAN_YEARLY_PRESCRIPTIONS)) +
    geom_histogram(binwidth = 100) +
    labs(
        title = "Histogram of Mean Yearly Past Prescriptions",
        x = "Mean Yearly Past Prescriptions",
    ) +
    plot_theme
mean_past_pres_hist

# Mean yearly past prescription by specialty
past_pres_by_specialty <- prescription_rate %>%
    filter(!is.na(SPECIALTY) & !is.na(MEAN_YEARLY_PRESCRIPTIONS)) %>%
    group_by(SPECIALTY) %>%
    summarize(
        MEAN_SPECIALTY_PRESCRIPTIONS = mean(MEAN_YEARLY_PRESCRIPTIONS),
        TOTAL = n()
    ) %>%
    arrange(desc(MEAN_SPECIALTY_PRESCRIPTIONS))

past_pres_by_specialty_plot <- ggplot(past_pres_by_specialty %>% filter(TOTAL > 1000), aes(x = reorder(SPECIALTY, MEAN_SPECIALTY_PRESCRIPTIONS), y = MEAN_SPECIALTY_PRESCRIPTIONS)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
        title = "Mean Yearly Past Prescriptions by Specialty",
        x = "Specialty",
        y = "Mean Yearly Past Prescriptions"
    ) +
    plot_theme
past_pres_by_specialty_plot

# Prescription rate by mean yearly past prescriptions
rate_by_past_pres <- prescription_rate %>%
    filter(!is.na(MEAN_YEARLY_PRESCRIPTIONS)) %>%
    mutate(
        MEAN_YEARLY_PRESCRIPTIONS_BIN = cut(
            MEAN_YEARLY_PRESCRIPTIONS,
            breaks = quantile(MEAN_YEARLY_PRESCRIPTIONS, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
            labels = paste0(seq(0, 90, by = 10), "-", seq(9, 99, by = 10)),
            include.lowest = TRUE,
            dig.lab = 4
        )
    ) %>%
    group_by(MEAN_YEARLY_PRESCRIPTIONS_BIN) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

rate_by_past_pres_plot <- ggplot(rate_by_past_pres, aes(x = MEAN_YEARLY_PRESCRIPTIONS_BIN, y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    labs(
        title = "Prescription Rate by Mean Yearly Past Prescriptions",
        x = "Mean Yearly Past Prescription Decile",
        y = "Prescription Rate (%)"
    ) +
    plot_theme
rate_by_past_pres_plot

# Scatter plot of doctor age vs. visit date, colored by prescription
age_doc_vs_visitdate_sample <- prescription_rate %>%
    filter(!is.na(AGE_AT_VISIT_DOC))

age_doc_vs_visitdate_plot <- ggplot(age_doc_vs_visitdate_sample, 
                                    aes(x = VISIT_DATE, y = AGE_AT_VISIT_DOC, color = factor(PRESCRIBED))) +
    geom_jitter(alpha = 0.5, width = 0.5, height = 0.5, size = 1) +
    scale_color_manual(
        values = c("0" = "#f4c0bd", "1" = "#91dddf"),
        labels = c("0" = "Did not prescribe", "1" = "Prescribed")
    ) +
    labs(
        title = "Doctor Age vs. Visit Date (J06.9)",
        x = "Visit Date",
        y = "Doctor Age",
        color = "Prescribed"
    ) +
    plot_theme
age_doc_vs_visitdate_plot

# Add indicator column for visits after the doctor information (age, specialty etc.) is available
cutoff_date <- as.Date("2016-01-01")
prescription_rate <- prescription_rate %>%
    mutate(AFTER_CUTOFF = VISIT_DATE > cutoff_date)

age_doc_pres_plot_group <- ggplot(prescription_rate %>% filter(!is.na(AGE_AT_VISIT_DOC)), aes(x = AGE_AT_VISIT_DOC, fill = factor(PRESCRIBED))) +
    geom_density(
        alpha = 0.4,
    ) +
    scale_fill_manual(
        values = c("0" = "#f4c0bd", "1" = "#91dddf"),
        labels = c("0" = "Did not prescribe", "1" = "Prescribed")
    ) +
    labs(
        title = "Age Distribution of Doctors Prescribing and not Prescribing to J06.9 Patients",
        x = "Age",
        y = "Probability density",
        fill = "Prescribed"
    ) +
    facet_wrap(~ AFTER_CUTOFF, labeller = as_labeller(c(`FALSE` = paste0("Visit Date ≤ ", cutoff_date), `TRUE` = paste0("Visit Date > ", cutoff_date)))) +
    plot_theme
age_doc_pres_plot_group

rate_by_secondary_diag <- prescription_rate %>%
    filter(!is.na(ICD10_CODE_2ND)) %>%
    group_by(ICD10_CODE_2ND) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n(),
        RELATIVE_FREQ = TOTAL / nrow(prescription_rate) * 100
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100) %>%
    add_binom_interval(count_col = "PRESCRIBED", n_col = "TOTAL")

rate_by_secondary_diag_common <- rate_by_secondary_diag %>%
    filter(RELATIVE_FREQ > 0.1)
print(paste0("Coverage of most common secondary diagnoses: ", round(sum(rate_by_secondary_diag_common$RELATIVE_FREQ), 1), "%"))

# Distribution of secondary diagnoses
secondary_diag_plot <- ggplot(rate_by_secondary_diag_common, aes(x = ICD10_CODE_2ND, y = RELATIVE_FREQ)) +
    geom_bar(stat = "identity") +
    labs(
        title = paste0("Distribution of Most Common Secondary Diagnoses for ", code_no_dot, " Patients"),
        x = "ICD10 Code",
        y = "Relative Frequency (%)",
    ) +
    plot_theme
secondary_diag_plot

# Prescription rate by secondary diagnoses
rate_by_secondary_diag_plot <- ggplot(rate_by_secondary_diag_common, aes(x = reorder(ICD10_CODE_2ND, PRESCRIBED_RATE), y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = LOWER_BOUND, ymax = UPPER_BOUND), width = 0.2) +
    labs(
        title = paste0(code_no_dot, " Prescription Rate by Secondary Diagnosis"),
        x = "Secondary Diagnosis",
        y = "Prescription Rate (%)"
    ) +
    plot_theme
rate_by_secondary_diag_plot

# Make a PDF summary of the most important plots
plot_summary_theme <- theme(
    plot.title = element_text(size = 24),
    axis.title = element_text(size = 21),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 19)
)

pdf(paste0(code_no_dot, "SummaryPlots.pdf"), width = 10, height = 7)
print(diag_by_year_pres_plot + plot_summary_theme)
print(yearly_rate_plot + plot_summary_theme)
print(class_freq_plot + plot_summary_theme)
print(rate_by_specialty_plot + plot_summary_theme)
print(age_doc_freq_plot + plot_summary_theme)
print(rate_by_age_doc_plot + plot_summary_theme)
print(rate_by_age_pat_plot + plot_summary_theme)
print(rate_by_sex_doc_plot + plot_summary_theme)
dev.off()