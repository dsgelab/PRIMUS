#### Load libraries 
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(lubridate)
library(stringr)
library(data.table)
library(R.utils)

#### Load data
ValviraFile <- "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
valvira <- fread(ValviraFile, encoding = "Latin-1")

glimpse(valvira)

valvira <- valvira %>% mutate(
    Ammattioikeus.voimassa.alkupäivämäärä = as.Date(Ammattioikeus.voimassa.alkupäivämäärä, format = "%d.%m.%Y"),
    Ammattioikeus.voimassa.loppupäivämäärä = as.Date(Ammattioikeus.voimassa.loppupäivämäärä, format = "%d.%m.%Y")
)
n_distinct(valvira$FID)

# Criteria for filtering 1: have a practicing license & can prescribe all medications (remove dentists)
valvira_licensed <- valvira %>% filter(
        str_detect(Ammattioikeus, "laillistettu erikoislääkäri") |
        str_detect(Ammattioikeus, "laillistettu lääkär") |
        str_detect(Ammattioikeus, "laillistettu lääkäri toisen johdon ja valvonnan alaisena") |
        str_detect(Ammattioikeus, "laillistettu yleislääkäri")
)

glimpse(valvira_licensed)
n_distinct(valvira_licensed$FID)

# Criteria for filtering 2: have a practicing license for at least 5 years AND from 1998
nrow(valvira_licensed %>% filter(is.na(Ammattioikeus.voimassa.alkupäivämäärä))) # 89 doctors without start date
# remove the doctors without start date
valvira_licensed <- valvira_licensed %>% filter(!is.na(Ammattioikeus.voimassa.alkupäivämäärä))
valvira_licensed_5years <- valvira_licensed %>%
    group_by(FID) %>%
    summarise(
        start_date = min(Ammattioikeus.voimassa.alkupäivämäärä, na.rm = TRUE),
        end_date = pmin(max(Ammattioikeus.voimassa.loppupäivämäärä, na.rm = TRUE), as.Date("2023-01-01")),
        license_periods = list(data.frame(start = Ammattioikeus.voimassa.alkupäivämäärä, end = pmin(Ammattioikeus.voimassa.loppupäivämäärä, as.Date("2023-01-01"))))
    ) %>%
    rowwise()

# Calculate practicing days
calculate_practicing_days <- function(license_periods) {
    license_periods <- bind_rows(list(license_periods)) %>% arrange(start)

    total_days <- 0
    previous_end <- NULL

    for (i in seq_len(nrow(license_periods))) {
        current_start <- license_periods$start[i]
        current_end <- license_periods$end[i]

        if (!is.null(previous_end)) {
            if (current_start <= previous_end) {
                if (current_end <= previous_end) {
                    next
                } else {
                    current_start <- previous_end
                }
            }
        }

        days <- as.numeric(difftime(current_end, current_start, units = "days")) + 1
        days <- max(days, 0)
        total_days <- total_days + days
        previous_end <- current_end
    }

    return(total_days)
}

valvira_licensed_5years <- valvira_licensed_5years %>%
    rowwise() %>%
    mutate(practicing_days = calculate_practicing_days(license_periods)) %>%
    ungroup()

valvira_licensed_5years <- valvira_licensed_5years %>%
    filter(practicing_days >= 1825) %>%
    filter(end_date >= as.Date("2003-01-01"))

n_distinct(valvira_licensed_5years$FID)


# Criteria for filtering 3: active doctor = treated (prescribed/diagnosed) at least 5 patients
dp_summary <- fread("/media/volume/Projects/mattferr/doctor_patient_summary_20250218.csv")
glimpse(dp_summary)

doctor_activity <- dp_summary %>%
    mutate(
        prescription_criteria = ifelse(Prescriptions >= 5, 1, 0),
        diagnosis_criteria = ifelse(`DiagnosisAvohilmo` + `DiagnosisHilmo` >= 5, 1, 0),
        active_doctor = ifelse(prescription_criteria == 1 | diagnosis_criteria == 1, 1, 0)
    ) %>%
    select(FID = DOCTOR_ID, prescription_criteria, diagnosis_criteria, active_doctor)

valvira_licensed_5years_active <- valvira_licensed_5years %>% left_join(doctor_activity, by = "FID") %>% filter(prescription_criteria == 1)
n_distinct(valvira_licensed_5years_active$FID)

# Criteria for filtering 4: doctor has BIRTHDATE and SEX information
dvv <- fread("/media/volume/Data/Data_THL_2698_14.02.00_2023/DVV/FD_2698_Tulokset_2024-04-09_HY.csv")
# we know from data exploration that there is 0 missingness in these columns

dvv <- dvv %>% select(FID, BIRTH_DATE = `Syntymä-päivä`, SEX = `Suku-.puoli`)

valvira_licensed_5years_active <- valvira_licensed_5years_active %>%
    left_join(dvv, by = "FID") %>%
    filter(!is.na(BIRTH_DATE) & !is.na(SEX))

# remove license periods column
valvira_licensed_5years_active <- valvira_licensed_5years_active %>%
    select(-license_periods)

# save only doctors 
doctors <- valvira_licensed_5years_active %>% select(FID)
fwrite(doctors, "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv", col.names = FALSE)


#### Add last specialty information to each doctor ####
# investigate the specialty information in the valvira dataset to find out whether there is a hierarchy in the specialty codes

specialties <- valvira_licensed %>%
    select(Tutkinto_Koodi, Tutkinto) %>%
    group_by(Tutkinto_Koodi, Tutkinto) %>%
    summarise(count = n()) %>%
    arrange(Tutkinto_Koodi)

specialties_count <- specialties %>%
    mutate(Tutkinto_Koodi_Prefix = ifelse(
        str_detect(Tutkinto_Koodi, "^[^-]+"),
        str_extract(Tutkinto_Koodi, "^[^-]+"),
        Tutkinto_Koodi
    )) %>%
    group_by(Tutkinto_Koodi_Prefix) %>%
    summarise(count = n()) %>%
    arrange(Tutkinto_Koodi_Prefix)

print(specialties)

# create a dataframe with the doctor's longest specialty (trimmed to the numbers before the dash)
longest_specialty <- valvira_licensed %>%
    mutate(
        Tutkinto_Koodi_Prefix = ifelse(
            str_detect(Tutkinto_Koodi, "^[^-]+"),
            str_extract(Tutkinto_Koodi, "^[^-]+"),
            Tutkinto_Koodi
        )
    ) %>%
    mutate(practicing_days = as.numeric(difftime(pmin(Ammattioikeus.voimassa.loppupäivämäärä, as.Date("2023-01-01")),
        Ammattioikeus.voimassa.alkupäivämäärä,
        units = "days"
    ))) %>%
    # get the practicing days for each doctor for each specialty
    group_by(FID, Tutkinto_Koodi_Prefix) %>%
    summarise(
        practicing_days = sum(practicing_days)
    ) %>%
    # pick the longest practicing specialty for each doctor
    arrange(FID, desc(practicing_days)) %>%
    slice(1) %>%
    select(FID, SPECIALTY = Tutkinto_Koodi_Prefix)

# merge longest speciality with the doctors dataset
valvira_licensed_5years_active <- valvira_licensed_5years_active %>%
    left_join(longest_specialty, by = "FID")

# with specialty dictionary
spec_dict <- read.csv("/media/volume/Projects/DSGELabProject1/condensed_specialty_dict.csv")

valvira_licensed <- valvira_licensed %>%
    mutate(Tutkinto_Koodi = as.integer(substr(Tutkinto_Koodi, 1, 5))) %>%
    left_join(spec_dict %>% select(-COMPRISED), by = c("Tutkinto_Koodi" = "CODEVALUE"))

# removing "NO SPECIALTY"
valvira_only_special <- valvira_licensed %>%
    mutate(LABEL_EN = ifelse(LABEL_EN %in% c("LICENSED_MED", "LICENSED_DENT", "STUDENT_MED", "STUDENT_DENT"), NA, LABEL_EN)) %>% 
    filter(!is.na(LABEL_EN))

# longest specialty w/o no specialty
last_specialty <- valvira_only_special %>%
    arrange(FID, desc(Ammattioikeus.voimassa.alkupäivämäärä)) %>%
    group_by(FID) %>%
    slice(1) %>%
    select(FID, LAST_SPECIALTY = LABEL_EN)

valvira_characteristics <- valvira_licensed_5years_active %>%
    mutate(SPECIALTY = is.integer(SPECIALTY)) %>%
    left_join(spec_dict %>% select(-COMPRISED), by = c("SPECIALTY" = "CODEVALUE")) %>%
    select(-SPECIALTY) %>% 
    dplyr::rename(LONGEST_SPECIALTY = LABEL_EN) %>% 
    mutate(LONGEST_SPECIALTY = ifelse(LONGEST_SPECIALTY %in% c("LICENSED_MED", "LICENSED_DENT", "STUDENT_MED", "STUDENT_DENT"), NA, LONGEST_SPECIALTY)) %>%
    left_join(last_specialty, by = "FID")

# QC this dataset
valvira_characteristics <- valvira_characteristics %>% mutate(BIRTH_DATE = ymd(BIRTH_DATE))

# save dataset
fwrite(valvira_characteristics, "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250424.csv")