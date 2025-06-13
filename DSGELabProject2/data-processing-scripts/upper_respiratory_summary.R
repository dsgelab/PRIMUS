library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
setwd("/media/volume/Projects/mikael/ProcessedData")
source("/media/volume/Projects/mikael/utils.R")

diagnosis_file <- get_latest_file("FirstConnectedJ069Diagnoses") # First diagnosis for each patient
prescription_file <- get_latest_file("J01Prescriptions")
doctor_file <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
patient_file <- "/media/volume/Data/Data_THL_2698_14.02.00_2023/DVV/FD_2698_Tulokset_2024-04-09_HY.csv"

current_date <- strftime(Sys.Date(), "%Y%m%d")

# diagnosis <- fread(get_latest_file("AllConnectedDiagnoses"))[startsWith(ICD10_CODE, "J06.9") | startsWith(ICD10_CODE, "J069")]
# # Only select the earliest instance of diagnosis for each patient
# diagnosis <- diagnosis %>%
#     as_tibble() %>%
#     arrange(PATIENT_ID, VISIT_DATE) %>%
#     group_by(PATIENT_ID) %>%
#     slice(1) %>%
#     ungroup()
# write.csv(diagnosis, paste0("FirstConnectedJ069Diagnoses_", current_date, ".csv"), row.names = FALSE)
diagnosis <- fread(diagnosis_file) %>%
    as_tibble() %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    select(PATIENT_ID, VISIT_DATE, ICD10_CODE, SOURCE, DOCTOR_ID)

count <- nrow(diagnosis)
print(paste("Number of patients (first diagnoses):", count))
count_with_doctor <- nrow(diagnosis %>% filter(!is.na(DOCTOR_ID)))
percentage_with_doctor <- sprintf("%.2f%%", count_with_doctor / count * 100)
print(paste0("Number of first diagnoses connected to a doctor: ", count_with_doctor, " (", percentage_with_doctor, ")"))

codes <- unique(diagnosis$ICD10_CODE)
print(paste("All ICD10 codes starting with J06.9:", paste(codes, collapse = ", ")))

plot_theme <- theme(
    plot.title = element_text(size = 28),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 20)
)

diagnosis %>%
    mutate(DIAGNOSIS_YEAR = format(VISIT_DATE, "%Y")) %>%
    count(DIAGNOSIS_YEAR, SOURCE) %>%
    ggplot(aes(x = DIAGNOSIS_YEAR, y = n, fill = SOURCE)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(breaks = ~ levels(factor(.x))[seq(1, nlevels(factor(.x)), by = 2)]) +
    labs(title = "Number of J06.9 Diagnoses by Year and Source Dataset",
         x = "Year",
         y = "Diagnoses",
         fill = "Source dataset") +
    plot_theme

# prescription <- fead(get_latest_file("AllConnectedPrescriptions"))[startsWith(ATC_CODE, "J01")]
# write.csv(prescription, paste0("J01Prescriptions_", current_date, ".csv"), row.names = FALSE)
prescription <- fread(prescription_file) %>%
    as_tibble() %>%
    select(PATIENT_ID, PRESCRIPTION_DATE, DOCTOR_ID)
print(paste("Number of J01 (antibiotics) prescriptions:", nrow(prescription)))

diag_pres <- diagnosis %>%
    inner_join(prescription, by = "PATIENT_ID", suffix = c("_DIAG", "_PRES")) %>%
    filter(PRESCRIPTION_DATE >= VISIT_DATE) %>%
    arrange(PATIENT_ID, PRESCRIPTION_DATE) %>%
    group_by(PATIENT_ID) %>%
    slice(1) %>%
    ungroup()

# Statistics about number of prescription. Same doctor means that the prescription and diagnosis were made by the same doctor.
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

ggplot(pstats, aes(x = DOCTOR_MATCH, y = COUNT, fill = PRESCRIPTION_TIME)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(
        title = "J06.9 Prescription Timing by Doctor Match",
        x = "Doctor Match",
        y = "Count",
        fill = "Prescription Time"
    ) +
    plot_theme

doctor <- fread(doctor_file) %>%
    as_tibble() %>%
    rename(SPECIALTY = INTERPRETATION) %>%
    mutate(SPECIALTY = replace(SPECIALTY, SPECIALTY == "" | is.na(SPECIALTY), "Licensed Doctor")) %>%
    select(DOCTOR_ID, PRACTICING_DAYS, BIRTH_DATE, SEX, SPECIALTY, BIRTH_MUNICIP_NAME, LANGUAGE)
patient <- fread(patient_file) %>%
    as_tibble() %>%
    rename(BIRTH_DATE = "Syntymä-päivä", SEX = "Suku-.puoli", PATIENT_ID = FID) %>%
    mutate(BIRTH_DATE = ymd(BIRTH_DATE)) %>%
    select(PATIENT_ID, BIRTH_DATE, SEX)

calc_age <- function(birth_date, current_date) {
    as.numeric(difftime(current_date, birth_date, units = "days") / 365.25)
}

# Summarizes all diagnoses and whether a prescription was made after the diagnosis. The prescription information is
# imputed by assigning a prescription to a patient who received an antibiotic prescription from the same doctor as the
# diagnosis on the same day. Prescriptions within a week from any doctor are classified as "unclear" (excluding same-doctor
# same-day prescriptions).
prescription_rate_init <- diagnosis %>%
    left_join(doctor, by = "DOCTOR_ID") %>%
    left_join(prescription, by = "PATIENT_ID", suffix = c("_DIAG", "_PRES")) %>%
    left_join(patient, by = "PATIENT_ID", suffix = c("_DOC", "_PAT")) %>%
    # Indicator for prescriptions and unclear prescriptions
    mutate(
        UNCLEAR_OR_PRES = !is.na(PRESCRIPTION_DATE) & (
            (as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) < 7 &
                 as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) >= 0)
        )
    ) %>%
    mutate(AGE_DOC = calc_age(BIRTH_DATE_DOC, VISIT_DATE)) %>%
    mutate(AGE_PAT = calc_age(BIRTH_DATE_PAT, VISIT_DATE)) %>%
    # Select only one row per patient, preferably the most recent prescription (if any)
    arrange(PATIENT_ID, desc(UNCLEAR_OR_PRES), PRESCRIPTION_DATE) %>%
    group_by(PATIENT_ID) %>%
    slice(1) %>%
    ungroup()

pr <- prescription_rate_init
prescribed_condition <- !is.na(pr$DOCTOR_ID_DIAG) & !is.na(pr$DOCTOR_ID_PRES) & pr$VISIT_DATE == pr$PRESCRIPTION_DATE & pr$DOCTOR_ID_DIAG == pr$DOCTOR_ID_PRES
n_prescribed <- prescription_rate_init %>% filter(prescribed_condition) %>% nrow()
n_not_prescribed <- prescription_rate_init %>%
    filter(
        is.na(PRESCRIPTION_DATE) |
            as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) >= 7 |
            PRESCRIPTION_DATE < VISIT_DATE
    ) %>%
    nrow()
# Get unclear prescriptions with formula sum(I(unclear or prescribed) - I(prescribed))
n_unclear <- prescription_rate_init %>% filter(UNCLEAR_OR_PRES == 1) %>% nrow() - n_prescribed
stopifnot(n_prescribed + n_not_prescribed + n_unclear == nrow(prescription_rate_init))
prescription_classes <- tibble(
    CLASS = c("Prescribed", "Not Prescribed", "Unclear"),
    COUNT = c(n_prescribed, n_not_prescribed, n_unclear),
) %>%
    mutate(PERCENTAGE = COUNT / sum(COUNT) * 100)
prescription_classes

ggplot(prescription_classes, aes(x = CLASS, y = COUNT, fill = CLASS)) +
    geom_bar(stat = "identity") +
    labs(
        title = "J06.9 Patients with Prescription vs No Prescription vs Unclear status",
        y = "Frequency",
        fill = "Label"
    ) +
    plot_theme

# Filter out unclear prescriptions from further analysis
prescription_rate <- prescription_rate_init %>%
    filter(prescribed_condition | UNCLEAR_OR_PRES == 0) %>%
    rename(PRESCRIBED = UNCLEAR_OR_PRES) %>%
    mutate(PRESCRIBED = as.numeric(PRESCRIBED))

n_unknown_doctor <- prescription_rate %>% filter(is.na(SPECIALTY)) %>% nrow()
percentage_unknown_doctor <- sprintf("%.2f%%", n_unknown_doctor / nrow(prescription_rate) * 100)
print(paste0("Number of patients with unknown doctor: ", n_unknown_doctor, " (", percentage_unknown_doctor, ")"))

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

ggplot(freq_by_specialty, aes(x = reorder(SPECIALTY, FREQUENCY), y = FREQUENCY, fill = FREQUENCY_TYPE)) +
    geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
    coord_flip() +
    labs(
        title = "Distribution of J06.9 Prescriptions and Diagnoses by Specialty",
        x = "Specialty",
        y = "Relative Frequency (%)",
        fill = "Frequency Type"
    ) +
    scale_fill_discrete(
        labels = c("DIAGNOSIS" = "Diagnosis", "PRESCRIPTION" = "Prescription")
    ) +
    plot_theme

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
    arrange(-TOTAL)
mean_prescription_rate <- mean(prescription_rate %>% filter(!is.na(SPECIALTY)) %>% pull(PRESCRIBED)) * 100

ggplot(rate_by_specialty, aes(x = reorder(SPECIALTY, PRESCRIBED_RATE), y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    geom_hline(
        yintercept = mean_prescription_rate,
        linetype = "dashed",
        color = "red"
    ) +
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
        y = "Prescription Rate"
    ) +
    plot_theme

yearly_rate <- prescription_rate %>%
    mutate(YEAR = lubridate::year(VISIT_DATE)) %>%
    group_by(YEAR) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100)

yearly_rate %>%
    ggplot(aes(x = YEAR, y = PRESCRIBED_RATE)) +
    geom_line() +
    labs(
        title = "J06.9 Prescription Rate over Time",
        x = "Year",
        y = "Prescription Rate"
    ) +
    plot_theme

# Histogram of ages
ggplot(prescription_rate %>% filter(!is.na(AGE_DOC)), aes(x = AGE_DOC, fill = factor(PRESCRIBED))) +
    geom_density(
        alpha = 0.4,
    ) +
    scale_fill_manual(
        values = c("0" = "#f4c0bd", "1" = "#91dddf"),
        labels = c("0" = "Did not presribe", "1" = "Prescribed")
    ) +
    labs(
        title = "Age Distribution of Doctors Prescribing and not Prescribing to J06.9 Patients",
        x = "Age",
        y = "Probability density",
        fill = "Prescribed"
    ) +
    plot_theme

rate_by_age <- prescription_rate %>%
    filter(!is.na(AGE_DOC)) %>%
    # Discretize doctor age to bins of 5 years
    mutate(
        AGE_BIN_DOC = cut(
            AGE_DOC,
            breaks = seq(from = floor(min(AGE_DOC) / 5) * 5, to = ceiling(max(AGE_DOC) / 5) * 5, by = 5),
            labels = paste0(seq(floor(min(AGE_DOC) / 5) * 5, ceiling(max(AGE_DOC) / 5) * 5 - 5, by = 5), "-",
                            seq(floor(min(AGE_DOC) / 5) * 5 + 4, ceiling(max(AGE_DOC) / 5) * 5 - 1, by = 5)),
            right = FALSE
        )
    ) %>%
    group_by(AGE_BIN_DOC) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100)

# Prescription rate by doctor age
ggplot(rate_by_age, aes(x = AGE_BIN_DOC, y = PRESCRIBED_RATE)) +
    geom_bar(stat = "identity") +
    labs(
        title = "J06.9 Prescription Rate by Doctor Age",
        x = "Age",
        y = "Prescription Rate"
    ) +
    plot_theme

rate_by_sex <- prescription_rate %>%
    filter(!is.na(SEX_DOC)) %>%
    group_by(SEX_DOC) %>%
    summarize(
        PRESCRIBED = sum(PRESCRIBED),
        TOTAL = n()
    ) %>%
    mutate(PRESCRIBED_RATE = PRESCRIBED / TOTAL * 100)

# Prescription rate by doctor sex
ggplot(rate_by_sex, aes(x = factor(SEX_DOC), y = PRESCRIBED_RATE, fill = factor(SEX_DOC))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(
        values = c("1" = "#f8766d", "2" = "#619cff"),
        labels = c("1" = "Male", "2" = "Female")
    ) +
    labs(
        title = "J06.9 Prescription Rate by Doctor Sex",
        fill = "Sex",
        x = "Sex",
        y = "Prescription Rate"
    ) +
    plot_theme +
    theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
    )

# Histogram of patient ages
ggplot(prescription_rate %>% filter(!is.na(AGE_PAT)), aes(x = AGE_PAT)) +
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

# Histogram of patient sexes
ggplot(prescription_rate %>% filter(!is.na(SEX_PAT)), aes(x = factor(SEX_PAT), fill = factor(SEX_PAT))) +
    geom_bar() +
    scale_fill_manual(
        values = c("1" = "#f8766d", "2" = "#619cff"),
        labels = c("1" = "Male", "2" = "Female")
    ) +
    labs(
        title = "Sex Distribution of Patients Diagnosed with J06.9",
        fill = "Sex",
        x = "Sex",
        y = "Frequency",
    ) +
    plot_theme +
    theme(
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
    )

# Histogram of ages
ggplot(prescription_rate %>% filter(!is.na(AGE_PAT)), aes(x = AGE_PAT, fill = factor(PRESCRIBED))) +
    geom_density(
        alpha = 0.4,
    ) +
    scale_fill_manual(
        values = c("0" = "#f4c0bd", "1" = "#91dddf"),
        labels = c("0" = "Did not presribe", "1" = "Prescribed")
    ) +
    labs(
        title = "Age Distribution of J06.9 Patients Being Prescribed and not Prescribed Antibiotics",
        x = "Age",
        y = "Probability density",
        fill = "Prescribed"
    ) +
    plot_theme