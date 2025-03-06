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

mpath <- "C:/Users/.../valvira/"
setwd(mpath)

valvira <- fread("valvira.csv", encoding = "Latin-1")

glimpse(valvira)

valvira <- valvira %>% mutate(
    Ammattioikeus.voimassa.alkupäivämäärä = as.Date(Ammattioikeus.voimassa.alkupäivämäärä, format = "%d.%m.%Y"),
    Ammattioikeus.voimassa.loppupäivämäärä = as.Date(Ammattioikeus.voimassa.loppupäivämäärä, format = "%d.%m.%Y")
)
n_distinct(valvira$FID) 
# Count of doctors: 46 097

# Criteria for filtering 1: have a practicing license
# laillistettu erikoishammaslääkäri or laillistettu erikoislääkäri or laillistettu hammaslääkäri or
# laillistettu lääkär or laillistettu lääkäri toisen johdon ja valvonnan alaisena or
# laillistettu yleislääkäri

valvira_licensed <- valvira %>% filter(
    str_detect(Ammattioikeus, "laillistettu erikoishammaslääkäri") |
        str_detect(Ammattioikeus, "laillistettu erikoislääkäri") |
        str_detect(Ammattioikeus, "laillistettu hammaslääkäri") |
        str_detect(Ammattioikeus, "laillistettu lääkär") |
        str_detect(Ammattioikeus, "laillistettu lääkäri toisen johdon ja valvonnan alaisena") |
        str_detect(Ammattioikeus, "laillistettu yleislääkäri")
)

glimpse(valvira_licensed)
n_distinct(valvira_licensed$FID)
# Count of doctors: 42 570

# Criteria for filtering 2: have a practicing license for at least 5 years AND from 1998

nrow(valvira_licensed %>% filter(is.na(Ammattioikeus.voimassa.alkupäivämäärä))) # 89 doctors without start date
# remove the doctors without start date
valvira_licensed <- valvira_licensed %>% filter(!is.na(Ammattioikeus.voimassa.alkupäivämäärä))
# Count of doctors: 42 570 (stays the same because doctors had another active license)

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
# Count of doctors: 37 126


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

valvira_licensed_5years_active <- valvira_licensed_5years %>% left_join(doctor_activity, by = "FID") %>%
    # filter(active_doctor == 1) # Count of doctors: 31 281
    filter(prescription_criteria == 1)
# Count of doctors: 30 995


# Criteria for filtering 4: doctor has BIRTHDATE and SEX information
dvv <- fread("DVV/FD_2698_Tulokset_2024-04-09_HY.csv")
# we know from data exploration that there is 0 missingness in these columns

dvv <- dvv %>% select(FID, BIRTH_DATE = `Syntymä-päivä`, SEX = `Suku-.puoli`)

valvira_licensed_5years_active <- valvira_licensed_5years_active %>%
    left_join(dvv, by = "FID") %>%
    filter(!is.na(BIRTH_DATE) & !is.na(SEX))
# Count of doctors: 30 542

# remove license periods column
valvira_licensed_5years_active <- valvira_licensed_5years_active %>%
    select(-license_periods)

# save dataset
fwrite(valvira_licensed_5years_active, "/media/volume/Projects/DSGELabProject1/doctor_characteristics_woSpecialty_20250220.csv")
# save only doctors 
doctors <- valvira_licensed_5years_active %>% select(FID)
fwrite(doctors, "/media/volume/Projects/DSGELabProject1/doctors_20250220.csv", col.names = FALSE)


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

# save dataset
fwrite(valvira_licensed_5years_active, "/media/volume/Projects/DSGELabProject1/doctor_characteristics_wlongest_Specialty_20250220.csv")

