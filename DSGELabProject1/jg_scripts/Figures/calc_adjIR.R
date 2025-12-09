#!/usr/bin/env Rscript

# Command line script to calculate age-sex adjusted incidence rates for all ICD codes
# Usage: Rscript calculate_incidence_rates.R <diag_wide_file> <standard_pop_file> [doctor_filter] [end_date]
# Example: Rscript calculate_incidence_rates.R diag_wide.csv standard_pop.csv 1 2022-12-31

library(dplyr)
library(Epi)
library(lubridate)
library(data.table)

# ============================================================================
# HARD-CODED DIRECTORIES
# ============================================================================

LOG_DIR <- "/media/volume/Projects/jg/Logs"      # CHANGE THIS TO YOUR DESIRED LOG DIRECTORY
OUTPUT_DIR <- "/media/volume/Projects/jg/Output_files"  # CHANGE THIS TO YOUR DESIRED OUTPUT DIRECTORY

# Create directories if they don't exist
if (!dir.exists(LOG_DIR)) {
  dir.create(LOG_DIR, recursive = TRUE)
}
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# ============================================================================
# LOGGING FUNCTIONS
# ============================================================================

log_file <- file.path(LOG_DIR, paste0("incidence_calculation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_error <- function(msg) {
  log_message(msg, level = "ERROR")
}

log_warning <- function(msg) {
  log_message(msg, level = "WARNING")
}

# ============================================================================
# PARSE COMMAND LINE ARGUMENTS
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  cat("Usage: Rscript calculate_incidence_rates.R <diag_wide_file> <standard_pop_file> [doctor_filter] [end_date]\n")
  cat("\nArguments:\n")
  cat("  diag_wide_file    : Path to diagnosis data file (CSV or RDS)\n")
  cat("  standard_pop_file : Path to standard population file (CSV or RDS), use 'NULL' to skip\n")
  cat("  doctor_filter     : Optional. 1=doctors, 0=non-doctors, NULL=all (default: 1)\n")
  cat("  end_date          : Optional. End of follow-up date YYYY-MM-DD (default: 2022-12-31)\n")
  cat("\nExample:\n")
  cat("  Rscript calculate_incidence_rates.R diag_wide.csv standard_pop.csv 1 2022-12-31\n")
  cat("\nOutput will be saved to:", OUTPUT_DIR, "\n")
  cat("Logs will be saved to:", LOG_DIR, "\n")
  quit(status = 1)
}

diag_wide_file <- args[1]
standard_pop_file <- args[2]
doctor_filter <- if (length(args) >= 3 && args[3] != "NULL") as.numeric(args[3]) else 1
end_date <- if (length(args) >= 4) as.Date(args[4]) else as.Date("2022-12-31")

# Generate output filename based on doctor_filter
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
if (is.null(doctor_filter)) {
  output_filename <- paste0("diag_all_results_", timestamp, ".csv")
} else if (doctor_filter == 1) {
  output_filename <- paste0("diag_docs_results_", timestamp, ".csv")
} else {
  output_filename <- paste0("diag_nondocs_results_", timestamp, ".csv")
}
output_file <- file.path(OUTPUT_DIR, output_filename)

log_message("========================================")
log_message("Starting ICD Incidence Rate Calculation")
log_message("========================================")
log_message(paste("Diagnosis data file:", diag_wide_file))
log_message(paste("Standard population file:", standard_pop_file))
log_message(paste("Output file:", output_file))
log_message(paste("Doctor filter:", ifelse(is.null(doctor_filter), "NULL (all)", doctor_filter)))
log_message(paste("End of follow-up:", end_date))
log_message(paste("Log file:", log_file))

# ============================================================================
# LOAD DATA
# ============================================================================

log_message("Loading diagnosis data...")
tryCatch({
  if (grepl("\\.rds$", tolower(diag_wide_file))) {
    diag_wide <- readRDS(diag_wide_file)
  } else if (grepl("\\.csv$", tolower(diag_wide_file))) {
    diag_wide <- fread(diag_wide_file)
  } else {
    stop("Unsupported file format. Use .csv or .rds")
  }
  log_message(paste("Loaded", nrow(diag_wide), "rows and", ncol(diag_wide), "columns"))
}, error = function(e) {
  log_error(paste("Failed to load diagnosis data:", e$message))
  quit(status = 1)
})

# Load standard population if provided
standard_pop <- NULL
if (standard_pop_file != "NULL") {
  log_message("Loading standard population data...")
  tryCatch({
    if (grepl("\\.rds$", tolower(standard_pop_file))) {
      standard_pop <- readRDS(standard_pop_file)
      standard_pop[standard_pop == "[55,60)"] <- "[55,60]"
    } else if (grepl("\\.csv$", tolower(standard_pop_file))) {
      standard_pop <- fread(standard_pop_file)
      standard_pop[standard_pop == "[55,60)"] <- "[55,60]"
    } else {
      stop("Unsupported file format. Use .csv or .rds")
    }
    log_message(paste("Loaded standard population with", nrow(standard_pop), "rows"))
    
    # Verify required columns
    required_cols <- c("SEX", "age_band", "weight")
    missing_cols <- setdiff(required_cols, names(standard_pop))
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns in standard population:", paste(missing_cols, collapse = ", ")))
    }
  }, error = function(e) {
    log_error(paste("Failed to load standard population:", e$message))
    log_warning("Proceeding with internal weights instead")
    standard_pop <- NULL
  })
} else {
  log_message("No standard population provided - using internal weights")
}

# ============================================================================
# MAIN CALCULATION FUNCTION
# ============================================================================

calculate_all_incidence_rates <- function(diag_wide, 
                                         standard_pop = NULL,
                                         end_of_data = as.Date("2022-12-31"),
                                         doctor_filter = 1) {
  
  # Filter based on is_doctor value
  if (!is.null(doctor_filter)) {
    df_base <- diag_wide %>% 
      filter(is_doctor == doctor_filter) %>%
      select(ID, SEX, BIRTH_DATE, DEATH_DATE, DEATH) %>%
      mutate(
        BIRTH_DATE = ymd(na_if(as.character(BIRTH_DATE), "")),
        DEATH_DATE = ymd(na_if(as.character(DEATH_DATE), ""))
      )
  } else {
    df_base <- diag_wide %>% 
      select(ID, SEX, BIRTH_DATE, DEATH_DATE, DEATH) %>%
      mutate(
        BIRTH_DATE = ymd(na_if(as.character(BIRTH_DATE), "")),
        DEATH_DATE = ymd(na_if(as.character(DEATH_DATE), ""))
      )
  }
  
  log_message(paste("Working with", nrow(df_base), "individuals after filtering"))
  
  # Identify all ICD code columns
  all_cols <- names(diag_wide)
  exclude_patterns <- c("BIRTH_DATE", "DEATH_DATE", "ENTRY_DATE", "EXIT_DATE", "ENTRY", "EXIT")
  date_cols <- all_cols[grepl("_DATE$", all_cols) & 
                        !all_cols %in% exclude_patterns]
  
  icd_codes <- sub("_DATE$", "", date_cols)
  log_message(paste("Found", length(icd_codes), "ICD codes to process"))
  
  # Initialize results
  results <- data.frame(
    ICD_CODE = character(),
    adj_IR_1k = numeric(),
    events = integer(),
    person_years = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each ICD code
  for (i in seq_along(icd_codes)) {
    icd <- icd_codes[i]
    
    if (i %% 10 == 0) {
      log_message(paste("Processing ICD code", i, "of", length(icd_codes), ":", icd))
    }
    
    tryCatch({
      date_col <- paste0(icd, "_DATE")
      
      # Create working dataframe
      if (!is.null(doctor_filter)) {
        df <- diag_wide %>%
          filter(is_doctor == doctor_filter) %>%
          select(ID, SEX, BIRTH_DATE, DEATH_DATE, DEATH, 
                 ICD_FLAG = all_of(icd), 
                 ICD_DATE = all_of(date_col)) %>%
          mutate(
            BIRTH_DATE = ymd(na_if(as.character(BIRTH_DATE), "")),
            DEATH_DATE = ymd(na_if(as.character(DEATH_DATE), "")),
            ICD_DATE = ymd(na_if(as.character(ICD_DATE), ""))
          )
      } else {
        df <- diag_wide %>%
          select(ID, SEX, BIRTH_DATE, DEATH_DATE, DEATH, 
                 ICD_FLAG = all_of(icd), 
                 ICD_DATE = all_of(date_col)) %>%
          mutate(
            BIRTH_DATE = ymd(na_if(as.character(BIRTH_DATE), "")),
            DEATH_DATE = ymd(na_if(as.character(DEATH_DATE), "")),
            ICD_DATE = ymd(na_if(as.character(ICD_DATE), ""))
          )
      }
      
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
      
      # Split by age bands (20 to 60 years)
      age_breaks <- seq(20, 60, by = 5)
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
        filter(!is.na(age_breaks)) %>% 
        group_by(SEX, age_band) %>%
        summarise(
          pyrs = sum(lex.dur),
          events = sum(lex.Cst == "no_event" & lex.Xst == "event"),
          .groups = "drop"
        ) %>%
        mutate(
          IR = events / pyrs
        )
      
      # Apply weights
      if (!is.null(standard_pop)) {
        L_tab <- L_tab %>%
          left_join(standard_pop %>% select(SEX, age_band, weight), 
                    by = c("SEX", "age_band"))
        
        if (any(is.na(L_tab$weight))) {
          log_warning(paste("Missing standard population weights for", icd, "- using internal weights"))
          L_tab <- L_tab %>%
            mutate(weight = if_else(is.na(weight), pyrs / sum(pyrs), weight))
        }
      } else {
        L_tab <- L_tab %>%
          mutate(weight = pyrs / sum(pyrs))
      }
      
      # Calculate adjusted incidence rate
      adj_IR_1k <- sum(L_tab$IR * L_tab$weight, na.rm = TRUE) * 1000
      total_events <- sum(L_tab$events)
      total_pyrs <- sum(L_tab$pyrs)
      
      # Add to results
      results <- rbind(results, data.frame(
        ICD_CODE = icd,
        adj_IR_1k = adj_IR_1k,
        events = total_events,
        person_years = total_pyrs
      ))
      
    }, error = function(e) {
      log_error(paste("Error processing ICD code", icd, ":", e$message))
      log_error(paste("Traceback:", paste(sys.calls(), collapse = " -> ")))
    })
  }
  
  # Sort by incidence rate
  results <- results %>%
    arrange(desc(adj_IR_1k))
  
  return(results)
}

# ============================================================================
# RUN CALCULATION
# ============================================================================

log_message("Starting incidence rate calculations...")

start_time <- Sys.time()

tryCatch({
  results <- calculate_all_incidence_rates(
    diag_wide = diag_wide,
    standard_pop = standard_pop,
    end_of_data = end_date,
    doctor_filter = doctor_filter
  )
  
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  log_message(paste("Calculation completed in", round(duration, 2), "minutes"))
  log_message(paste("Processed", nrow(results), "ICD codes"))
  
  # Save results
  log_message(paste("Saving results to:", output_file))
  fwrite(results, output_file)
  log_message("Results saved successfully")
  
  # Print summary
  log_message("========================================")
  log_message("SUMMARY")
  log_message("========================================")
  log_message(paste("Total ICD codes:", nrow(results)))
  log_message(paste("Top 5 highest incidence rates:"))
  for (i in 1:min(5, nrow(results))) {
    log_message(sprintf("  %s: %.2f per 1000 PY (%d events, %.0f person-years)", 
                       results$ICD_CODE[i], 
                       results$adj_IR_1k[i],
                       results$events[i],
                       results$person_years[i]))
  }
  log_message("========================================")
  log_message("Process completed successfully!")
  
}, error = function(e) {
  log_error(paste("Fatal error during calculation:", e$message))
  log_error(paste("Traceback:", paste(sys.calls(), collapse = " -> ")))
  quit(status = 1)
})