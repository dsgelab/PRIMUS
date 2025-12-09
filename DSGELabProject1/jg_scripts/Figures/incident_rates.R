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
library(readxl)
library(gridExtra)
library(survival)
# .libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(gtsummary)
library(gt)
library(grid)
library(broom)
library(Epi)



# Load min phen diagnoses and medication data
min_phen_diagnoses <- fread("path/to/min_phen_diagnoses.csv")
min_phen_medications <- fread("path/to/min_phen_medications.csv")

# Load supporting data
doctor_IDs <- fread("/path/to/doctor_IDs.csv", header = F)
doctor_characteristics <- fread("/path/to/doctor_characteristics.csv")
dvv <- fread("/path/to/dvv.csv") 

# Preprocess the data
dvv <- dvv %>% rename(ID = FID,
                     BIRTH_DATE = `Syntymä-päivä`,
                     DEATH_DATE = Kuolinpäivä,
                     SEX = `Suku-.puoli`) %>% 
                select(ID, SEX, BIRTH_DATE, DEATH_DATE) %>% 
                mutate(BIRTH_DATE = as.Date(as.character(BIRTH_DATE), format = "%Y%m%d"),
                       DEATH_DATE = as.Date(as.character(DEATH_DATE), format = "%Y%m%d"))

dvv <- dvv %>%
    mutate(
        is_doctor = ifelse(ID %in% doctor_IDs$V1, 1, 0),
        age_end_of_FU = round(as.numeric(difftime(
            dplyr::if_else(is.na(DEATH_DATE), as.Date("2022-12-31"), DEATH_DATE),
            BIRTH_DATE,
            units = "days")) / 365.25, 2),
        DEATH = ifelse(is.na(DEATH_DATE), 0, 1)
    )


data.table::setnames(
    min_phen_diagnoses,
    old = names(min_phen_diagnoses)[1:6],
    new = c("PATIENT_ID", "VISIT_DATE", "ICD10_CODE", "SOURCE", "FD_HASH_CODE", "DOCTOR_ID")
)

# Parse VISIT_DATE to Date (ISO YYYY-MM-DD)
min_phen_diagnoses[, VISIT_DATE := as.IDate(VISIT_DATE, format = "%Y-%m-%d")]

# Keep only relevant columns
min_phen_diagnoses <- min_phen_diagnoses %>%
    select(PATIENT_ID, VISIT_DATE, ICD10_CODE)  %>% 
    filter(!is.na(VISIT_DATE))

# Trim ICD codes to 3 characters and keep only the first occurrence per patient/ICD10_CODE by VISIT_DATE
min_phen_diagnoses[, ICD10_CODE := substr(ICD10_CODE, 1, 3)]
min_phen_diagnoses <- min_phen_diagnoses %>%
    arrange(PATIENT_ID, VISIT_DATE) %>%
    distinct(PATIENT_ID, ICD10_CODE, .keep_all = TRUE)
gc()

# Doctor's diagnoses only
doctors_diagnoses <- min_phen_diagnoses %>%
    filter(PATIENT_ID %in% doctor_IDs$V1)

# Overall Population's diagnoses only
overall_population_diagnoses <- min_phen_diagnoses %>%
    filter(!PATIENT_ID %in% doctor_IDs$V1)


# Preprocess min_phen_medications
min_phen_medications <- min_phen_medications %>%
    select(PATIENT_ID, ATC_CODE = CODE, PRESCRIPTION_DATE) %>%
    filter(
        !is.na(PRESCRIPTION_DATE),
        PRESCRIPTION_DATE >= as.Date("1998-01-01"),
        PRESCRIPTION_DATE <= as.Date("2022-12-31")
    )


# Calculate incidence rate (age and sex adjusted) for doctors - Diagnoses

# create a df from min_phen_diagnoses and dvv with columns ID, SEX, BIRTH_DATE, DEATH_DATE (from dvv), and for every ICD10_CODE three columns: (1) a column with 1 if the doctor has that diagnosis, 0 otherwise (e.g. K21); (2) a column with diagnosis date (or NA if no diagnosis) (e.g. K21_DATE); (3)  a column with follow-up time at diagnosis (or follow-up time until end of follow-up/death if no diagnosis) (e.g. K21_TIME)
# create this df for all and only then split by doctors and overall population
# pivot to wide: columns like K21_DATE from VISIT_DATE (no summarise needed because earliest already present)
diag_wide <- min_phen_diagnoses %>%
    select(PATIENT_ID, ICD10_CODE, VISIT_DATE) %>%
    tidyr::pivot_wider(
        id_cols = PATIENT_ID,
        names_from = ICD10_CODE,
        values_from = VISIT_DATE,
        names_glue = "{ICD10_CODE}_DATE"
    ) %>%
    # add binary indicator (1/0) for each *_DATE column and place it right after the date column
    {
      df <- .
      date_cols <- grep("_DATE$", names(df), value = TRUE)
      if (length(date_cols) > 0) {
        for (dc in date_cols) {
          ind <- sub("_DATE$", "", dc)
          df[[ind]] <- as.integer(!is.na(df[[dc]]))
          df <- dplyr::relocate(df, dplyr::all_of(ind), .after = dplyr::all_of(dc))
        }
      }
      df
    }
gc()

fwrite(diag_wide, file = "path/to/diag_wide.csv")
diag_wide2 <- dvv %>% left_join(diag_wide, by = c("ID" = "PATIENT_ID"))
fwrite(diag_wide2, file = "path/to/diag_wide2.csv")


# Test Example of age-sex adjusted incidence rate calculation

df <- diag_wide2 %>% 
    select(1:9) %>% 
    filter(is_doctor == 1)  %>%
    mutate(across(ends_with("_DATE"), ~ lubridate::ymd(na_if(as.character(.), "")))) 

END_OF_DATA <- as.Date("2022-12-31")

df2 <- df %>%
    mutate(
        ENTRY_DATE = pmax(as.Date("1998-01-01"), BIRTH_DATE, na.rm = TRUE),

        # If K21 == 1, use K21_DATE; otherwise set far future
        tmp_k21 = dplyr::if_else(K21 == 1, K21_DATE, as.Date("2100-01-01")),
        # If DEATH == 1, use DEATH_DATE; otherwise set far future
        tmp_death = dplyr::if_else(DEATH == 1, DEATH_DATE, as.Date("2100-01-01")),

        # EXIT is earliest of event, death, or censoring date
        EXIT_DATE = pmin(tmp_k21, tmp_death, END_OF_DATA, na.rm = TRUE),

        EVENT = as.integer(K21 == 1 & !is.na(K21_DATE) & K21_DATE <= EXIT_DATE)
    ) %>%
    select(-tmp_k21, -tmp_death)

# Build Lexis object
L <- Lexis(
  entry = list(
    age = as.numeric(ENTRY_DATE - BIRTH_DATE) / 365.25
  ),
  exit = list(
    age = as.numeric(EXIT_DATE - BIRTH_DATE) / 365.25
  ),
  exit.status = factor(EVENT, levels=c(0,1), labels=c("noK21","K21")),
  data = df2,
  id = ID
)

age_breaks <- seq(0, 100, by = 5)

L_split <- splitLexis(L,
                      breaks = list(age = age_breaks),
                      time.scale = "age")

L_tab <- L_split %>%
  mutate(
    age_band = cut(
      age,
      breaks = age_breaks,
      right = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  group_by(SEX, age_band) %>%
  summarise(
    pyrs   = sum(lex.dur),  # person-years in that sex × age-band
    events = sum(lex.Cst == "noK21" & lex.Xst == "K21"),
    .groups = "drop"
  ) %>%
  mutate(
    IR = events / pyrs,         # crude incidence rate for that stratum
    IR_1k = IR * 1000           # per 1,000 person-years (if you like)
  )

L_tab <- L_tab %>%
  mutate(weight = pyrs / sum(pyrs))   # weights over all age × sex strata

adj_IR   <- sum(L_tab$IR * L_tab$weight)
adj_IR_1k <- adj_IR * 1000   # per 1,000 PY
adj_IR_1k



# Function to compute age- and sex-adjusted incidence (per 1,000 PY) for every ICD code
# Requires Epi and lubridate packages. Input diag_wide2 should be like in your script:
# - contains ID, SEX, BIRTH_DATE, DEATH_DATE, is_doctor (1/0) and for each ICD code columns: <ICD>_DATE (Date) and optionally <ICD> (0/1).
# Returns tibble with ICD_CODE and adj_IR_1k
calculate_all_incidence_rates <- function(diag_wide, end_of_data = as.Date("2022-12-31")) {
  
  # Filter to doctors only and prepare base data
  df_base <- diag_wide %>% 
    filter(is_doctor == 1) %>%
    select(ID, SEX, BIRTH_DATE, DEATH_DATE, DEATH) %>%
    mutate(
      BIRTH_DATE = ymd(na_if(as.character(BIRTH_DATE), "")),
      DEATH_DATE = ymd(na_if(as.character(DEATH_DATE), ""))
    )
  
  # Identify all ICD code columns (those with corresponding _DATE columns)
  all_cols <- names(diag_wide)
  # Exclude BIRTH_DATE, DEATH_DATE, and any ENTRY/EXIT related columns
  exclude_patterns <- c("BIRTH_DATE", "DEATH_DATE", "ENTRY_DATE", "EXIT_DATE", "ENTRY", "EXIT")
  date_cols <- all_cols[grepl("_DATE$", all_cols) & 
                        !all_cols %in% exclude_patterns]
  
  # Extract ICD codes (remove _DATE suffix)
  icd_codes <- sub("_DATE$", "", date_cols)
  
  # Initialize results data frame
  results <- data.frame(
    ICD_CODE = character(),
    adj_IR_1k = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each ICD code
  for (icd in icd_codes) {
    
    tryCatch({
      date_col <- paste0(icd, "_DATE")
      
      # Create working dataframe for this ICD code
      # Select only the needed columns from diag_wide
      df <- diag_wide %>%
        filter(is_doctor == 1) %>%
        select(ID, SEX, BIRTH_DATE, DEATH_DATE, DEATH, 
               ICD_FLAG = all_of(icd), 
               ICD_DATE = all_of(date_col)) %>%
        mutate(
          BIRTH_DATE = ymd(na_if(as.character(BIRTH_DATE), "")),
          DEATH_DATE = ymd(na_if(as.character(DEATH_DATE), "")),
          ICD_DATE = ymd(na_if(as.character(ICD_DATE), ""))
        )
      
      # Calculate entry and exit dates
      df2 <- df %>%
        mutate(
          ENTRY_DATE = pmax(as.Date("1998-01-01"), BIRTH_DATE, na.rm = TRUE),
          tmp_icd = if_else(ICD_FLAG == 1, ICD_DATE, as.Date("2100-01-01")),
          tmp_death = if_else(DEATH == 1, DEATH_DATE, as.Date("2100-01-01")),
          EXIT_DATE = pmin(tmp_icd, tmp_death, end_of_data, na.rm = TRUE),
          EVENT = as.integer(ICD_FLAG == 1 & !is.na(ICD_DATE) & ICD_DATE <= EXIT_DATE)
        ) %>%
        select(-tmp_icd, -tmp_death)
      
      # Build Lexis object
      L <- Lexis(
        entry = list(
          age = as.numeric(ENTRY_DATE - BIRTH_DATE) / 365.25
        ),
        exit = list(
          age = as.numeric(EXIT_DATE - BIRTH_DATE) / 365.25
        ),
        exit.status = factor(EVENT, levels = c(0, 1), labels = c("no_event", "event")),
        data = df2,
        id = ID
      )
      
      # Split by age bands
      age_breaks <- seq(0, 100, by = 5)
      L_split <- splitLexis(L,
                            breaks = age_breaks,
                            time.scale = "age")
      
      # Calculate incidence rates by age and sex
      L_tab <- L_split %>%
        mutate(
          age_band = cut(
            age,
            breaks = age_breaks,
            right = FALSE,
            include.lowest = TRUE
          )
        ) %>%
        group_by(SEX, age_band) %>%
        summarise(
          pyrs = sum(lex.dur),
          events = sum(lex.Cst == "no_event" & lex.Xst == "event"),
          .groups = "drop"
        ) %>%
        mutate(
          IR = events / pyrs,
          weight = pyrs / sum(pyrs)
        )
      
      # Calculate adjusted incidence rate
      adj_IR_1k <- sum(L_tab$IR * L_tab$weight) * 1000
      
      # Add to results
      results <- rbind(results, data.frame(
        ICD_CODE = icd,
        adj_IR_1k = adj_IR_1k
      ))
      
    }, error = function(e) {
      warning(paste("Error processing ICD code", icd, ":", e$message))
    })
  }
  
  # Sort by incidence rate (descending)
  results <- results %>%
    arrange(desc(adj_IR_1k))
  
  return(results)
}

# Usage example:
# result_table <- calculate_all_incidence_rates(diag_wide)
# print(result_table)