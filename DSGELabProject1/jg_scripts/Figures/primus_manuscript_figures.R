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

# Plot directory
plot_dir <- "/path/to/plots"

# Create timestamp for file names
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")


# Table 1: Demographics and Baseline Characteristics of Doctors Cohorts ----
# Table contains:
# N Doctors, Mean Birthyear (SD), N Female (%), Mean/Median Follow-up Time (SD/IQR), N Top 5 Specialties (%), Mean/Median Yearly Prescriptions (SD/IQR)


# Load the data
doctor_IDs <- fread("/path/to/doctor_IDs.csv", header = F)
doctor_characteristics <- fread("/path/to/doctor_characteristics.csv")
prescription_counts <- fread("/path/to/doctor_prescriptions_by_year.csv") # columns: DOCTOR_ID, YEAR, COUNT

# Preprocess the data
doctor_characteristics <- doctor_characteristics %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(
        BIRTH_YEAR = as.numeric(year(as.Date(BIRTH_DATE))),
        BIRTH_YEAR = ifelse(BIRTH_YEAR < 1900 | BIRTH_YEAR > 2023, NA, BIRTH_YEAR)
    ) %>% 
    mutate(START_YEAR = year(as.Date(START_DATE)))

prescription_counts <- prescription_counts %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(YEAR = as.numeric(YEAR)) %>% 
    filter(YEAR >= 1998 & YEAR <= 2022)

# Calculate N
n_doctors <- nrow(doctor_characteristics)

# Calculate Mean Birthyear (SD)
mean_birthyear <- round(mean(doctor_characteristics$BIRTH_YEAR, na.rm = TRUE), 2)
sd_birthyear <- round(sd(doctor_characteristics$BIRTH_YEAR, na.rm = TRUE), 2)
median_birthyear <- round(median(doctor_characteristics$BIRTH_YEAR, na.rm = TRUE), 2)
iqr_birthyear <- round(IQR(doctor_characteristics$BIRTH_YEAR, na.rm = TRUE), 2)

# Calculate Sex Distribution
sex_distribution <- doctor_characteristics %>%
    summarize(
        n_female = sum(SEX == 2, na.rm = TRUE),
        pct_female = round(100 * sum(SEX == 2, na.rm = TRUE) / sum(!is.na(SEX)),2)
    )

# Calculate Follow-up Time
doctor_characteristics <- doctor_characteristics %>%
    mutate(
        START_DATE = lubridate::ymd(START_DATE),
        END_DATE = lubridate::ymd(END_DATE),
        BIRTH_DATE = lubridate::ymd(BIRTH_DATE)
    ) %>%
    mutate(
        # enforce minimum start of 1998-01-01
        start_adj = pmax(START_DATE, as.Date("1998-01-01"), na.rm = TRUE),
        # compute 60th birthday from BIRTH_DATE if available, otherwise from BIRTH_YEAR (assume Jan 1)
        sixty_bday = dplyr::case_when(
            !is.na(BIRTH_DATE) ~ BIRTH_DATE + lubridate::years(60),
            !is.na(BIRTH_YEAR) ~ as.Date(paste0(BIRTH_YEAR + 60, "-01-01")),
            TRUE ~ as.Date(NA_character_)
        ),
        # end is the earlier of END_DATE and 60th birthday (if either missing, use the one available)
        end_adj = dplyr::case_when(
            !is.na(END_DATE) & !is.na(sixty_bday) ~ pmin(END_DATE, sixty_bday),
            !is.na(END_DATE) ~ END_DATE,
            !is.na(sixty_bday) ~ sixty_bday,
            TRUE ~ as.Date(NA_character_)
        ),
        # compute follow-up in years; if end is before start set to 0, if missing set to NA
        FOLLOWUP_YEARS = as.numeric(difftime(end_adj, start_adj, units = "days")) / 365.25,
        FOLLOWUP_YEARS = dplyr::case_when(
            is.na(FOLLOWUP_YEARS) ~ NA_real_,
            FOLLOWUP_YEARS < 0 ~ 0,
            TRUE ~ FOLLOWUP_YEARS
        )
    ) %>%
    select(-start_adj, -sixty_bday, -end_adj)


mean_followup <- round(mean(doctor_characteristics$FOLLOWUP_YEARS, na.rm = TRUE), 2)
sd_followup <- round(sd(doctor_characteristics$FOLLOWUP_YEARS, na.rm = TRUE), 2)
median_followup <- round(median(doctor_characteristics$FOLLOWUP_YEARS, na.rm = TRUE), 2)
iqr_followup <- round(IQR(doctor_characteristics$FOLLOWUP_YEARS, na.rm = TRUE), 2)

top_specialties <- doctor_characteristics %>%
    filter(!is.na(LAST_SPECIALTY) & LAST_SPECIALTY != "") %>%
    count(LAST_SPECIALTY, name = "n") %>%
    arrange(desc(n)) %>%
    slice_head(n = 5) %>%
    mutate(
        LAST_SPECIALTY = c(
            "General Medicine",
            "Surgery",
            "Internal Medicine",
            "Psychiatry",
            "Anesthesiology"
        )[row_number()],
        pct = round(100 * n / n_doctors, 2)
    )


# Yearly Prescriptions: compute per-doctor annualized prescription rate based on total prescriptions over follow-up days
# Aggregate prescriptions per doctor/year first (collapse duplicate rows)
prescription_counts <- prescription_counts %>%
    group_by(DOCTOR_ID, YEAR) %>%
    summarize(COUNT = sum(COUNT, na.rm = TRUE), .groups = "drop")

# Join doctor dates and follow-up info
presc_with_docs <- prescription_counts %>%
    left_join(
        doctor_characteristics %>% select(DOCTOR_ID, START_DATE, END_DATE, BIRTH_DATE, FOLLOWUP_YEARS),
        by = "DOCTOR_ID"
    ) %>%
    mutate(
        START_DATE = lubridate::ymd(START_DATE),
        END_DATE = lubridate::ymd(END_DATE),
        BIRTH_DATE = lubridate::ymd(BIRTH_DATE),
        # enforce minimum start of 1998-01-01 (same logic as earlier)
        start_adj = pmax(START_DATE, as.Date("1998-01-01"), na.rm = TRUE),
        sixty_bday = dplyr::case_when(
            !is.na(BIRTH_DATE) ~ BIRTH_DATE + lubridate::years(60),
            TRUE ~ as.Date(NA_character_)
        ),
        end_adj = dplyr::case_when(
            !is.na(END_DATE) & !is.na(sixty_bday) ~ pmin(END_DATE, sixty_bday),
            !is.na(END_DATE) ~ END_DATE,
            !is.na(sixty_bday) ~ sixty_bday,
            TRUE ~ as.Date(NA_character_)
        ),
        presc_year = as.integer(YEAR)
    ) %>%
    # keep prescriptions that fall within the doctor's follow-up period (by year)
    filter(
        !is.na(presc_year),
        dplyr::case_when(
            !is.na(start_adj) & !is.na(end_adj) ~ presc_year >= lubridate::year(start_adj) & presc_year <= lubridate::year(end_adj),
            !is.na(start_adj) ~ presc_year >= lubridate::year(start_adj),
            !is.na(end_adj) ~ presc_year <= lubridate::year(end_adj),
            TRUE ~ FALSE
        )
    )

# Sum total prescriptions per doctor during follow-up
per_doctor_total <- presc_with_docs %>%
    group_by(DOCTOR_ID) %>%
    summarize(total_prescriptions = sum(COUNT, na.rm = TRUE), .groups = "drop") %>%
    left_join(doctor_characteristics %>% select(DOCTOR_ID, FOLLOWUP_YEARS), by = "DOCTOR_ID") %>%
    mutate(
        followup_days = FOLLOWUP_YEARS * 365.25,
        # annualized prescriptions = (total prescriptions / follow-up days) * 365.25
        yearly_rate = dplyr::case_when(
            is.na(followup_days) ~ NA_real_,
            followup_days <= 0 ~ NA_real_,
            TRUE ~ (total_prescriptions / followup_days) * 365.25
        )
    )

# Use per_doctor_total as per_doctor_presc for downstream summary
per_doctor_presc <- per_doctor_total

# Overall summaries across doctors (mean/SD and median/IQR of per-doctor annualized rates)
mean_yearly_prescriptions <- round(mean(per_doctor_presc$yearly_rate, na.rm = TRUE), 2)
sd_yearly_prescriptions   <- round(sd(per_doctor_presc$yearly_rate, na.rm = TRUE), 2)
median_yearly_prescriptions <- round(median(per_doctor_presc$yearly_rate, na.rm = TRUE), 2)
iqr_yearly_prescriptions  <- round(IQR(per_doctor_presc$yearly_rate, na.rm = TRUE), 2)

# Formatted string for insertion into the table (Mean (SD); Median (IQR))
yearly_prescriptions_value <- paste0(
    mean_yearly_prescriptions, " (", sd_yearly_prescriptions, "); ",
    median_yearly_prescriptions, " (", iqr_yearly_prescriptions, ")"
)


# Create Table 1 with separate rows for yearly prescriptions mean and median
table1 <- tibble(
    Characteristic = c(
        "N Doctors",
        "Mean Birthyear (SD)",
        "Median Birthyear (IQR)",
        "N Female (%)",
        "Mean Follow-up Time (SD)",
        "Median Follow-up Time (IQR)",
        "Mean Yearly Prescriptions (SD)",
        "Median Yearly Prescriptions (IQR)",
        "Top 5 Specialties"
    ),
    Value = c(
        n_doctors,
        paste0(mean_birthyear, " (", sd_birthyear, ")"),
        paste0(median_birthyear, " (", iqr_birthyear, ")"),
        paste0(sex_distribution$n_female, " (", sex_distribution$pct_female, "%)"),
        paste0(mean_followup, " (", sd_followup, ")"),
        paste0(median_followup, " (", iqr_followup, ")"),
        paste0(mean_yearly_prescriptions, " (", sd_yearly_prescriptions, ")"),
        paste0(median_yearly_prescriptions, " (", iqr_yearly_prescriptions, ")"),
        paste0(top_specialties$LAST_SPECIALTY, " (", top_specialties$pct, "%)", collapse = ", ")
    )
)

# Build a compact table with indented specialties and percentages in Value
table1_base <- table1 %>% slice(1:8)

special_header <- tibble(Characteristic = "Top 5 Specialties (%)", Value = "")

indent <- "\u00A0\u00A0\u00A0"
special_rows <- top_specialties %>%
    transmute(
        Characteristic = paste0(indent, LAST_SPECIALTY),
        Value = paste0(n, " (", pct, "%)")
    )

table1_expanded <- bind_rows(table1_base, special_header, special_rows)

# FUNCTION to add commas to all numbers in a string (handles numbers inside parentheses too)
add_commas_to_all_numbers <- function(x) {
    out <- as.character(x)
    out[is.na(out)] <- NA_character_
    fmt_fun <- function(m) {
        num <- suppressWarnings(as.numeric(m))
        if (is.na(num)) return(m)
        prettyNum(num, big.mark = ",", scientific = FALSE, trim = TRUE)
    }
    out <- vapply(out, function(s) {
        if (is.na(s)) return(NA_character_)
        stringr::str_replace_all(s, "\\d+\\.?\\d*", function(m) fmt_fun(m))
    }, FUN.VALUE = character(1))
    out
}

# apply comma formatting to all numeric substrings in the Value column
table1_expanded$Value <- add_commas_to_all_numbers(table1_expanded$Value)

# Create and save gt table
table1_gt <- table1_expanded %>%
    gt() %>%
    tab_header(
        title = md("**Table 1. Demographics and Baseline Characteristics of Doctors' Cohort**"),
        subtitle = md(paste0("N = ", n_doctors))
    ) %>%
    cols_label(Characteristic = "Characteristic", Value = "Value") %>%
    fmt_missing(columns = everything(), missing_text = "") %>%
    cols_align(columns = vars(Characteristic), align = "left") %>%
    cols_align(columns = vars(Value), align = "right") %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything())) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = vars(Characteristic), rows = Characteristic == "Top 5 Specialties (%)")
    ) %>%
    tab_source_note(source_note = "Values are presented as mean (SD) or median (IQR) where appropriate. Percentages are over non-missing values.") %>%
    tab_options(table.font.size = 12, 
                heading.title.font.size = 14, 
                heading.subtitle.font.size = 11,
                data_row.padding = px(4),
                row.striping.background_color = "#F7F7F7")

# Save table1_gt with filepath, pdf() dev.off()
output_file_table1 <- file.path(plot_dir, paste0("Table_1_demographics_", timestamp, ".pdf"))
# Save table1_gt as PDF
gtsave(table1_gt, filename = output_file_table1)

# Save table1 as CSV
output_file_table1_csv <- file.path(plot_dir, paste0("Table_1_demographics_", timestamp, ".csv"))
fwrite(table1_expanded, file = output_file_table1_csv)


















# Figure 2: Disease / Medication Incidence Rate in the Cohort ----
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