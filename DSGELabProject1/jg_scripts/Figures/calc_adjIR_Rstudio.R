library(dplyr)
library(Epi)
library(lubridate)
library(data.table)

# ============================================================================
# PARAMETERS - MODIFY THESE
# ============================================================================

# Input files
diag_wide_file <- "path/to/diag_wide.csv"  # CHANGE THIS
standard_pop_file <- "path/to/standard_pop.csv"  # CHANGE THIS (or set to NULL if not using)

# Directories
LOG_DIR <- "/media/volume/Projects/jg/Logs"      # CHANGE THIS TO YOUR DESIRED LOG DIRECTORY
OUTPUT_DIR <- "/media/volume/Projects/jg/Output_files"  # CHANGE THIS TO YOUR DESIRED OUTPUT DIRECTORY

# Analysis parameters
doctor_filter <- 1  # 1 = doctors, 0 = non-doctors, NULL = all
end_date <- as.Date("2022-12-31")  # End of follow-up

# ============================================================================
# DO NOT MODIFY BELOW THIS LINE
# ============================================================================

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
log_message(paste("Standard population file:", ifelse(is.null(standard_pop_file), "NULL (using internal weights)", standard_pop_file)))
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
  stop(e)
})

# Load standard population if provided
standard_pop <- NULL
if (!is.null(standard_pop_file)) {
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
    ci_lower = numeric(),
    ci_upper = numeric(),
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
        filter(!is.na(age_band)) %>%  # Remove NA age bands
        group_by(SEX, age_band) %>%
        summarise(
          pyrs = sum(lex.dur),
          events = sum(lex.Cst == "no_event" & lex.Xst == "event"),
          .groups = "drop"
        ) %>%
        mutate(
          IR = events / pyrs,
          # Calculate 95% CI for each stratum using Poisson approximation
          IR_lower = qchisq(0.025, 2 * events) / (2 * pyrs),
          IR_upper = qchisq(0.975, 2 * (events + 1)) / (2 * pyrs)
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
      
      # Calculate adjusted incidence rate and confidence intervals
      adj_IR_1k <- sum(L_tab$IR * L_tab$weight, na.rm = TRUE) * 1000
      adj_IR_lower <- sum(L_tab$IR_lower * L_tab$weight, na.rm = TRUE) * 1000
      adj_IR_upper <- sum(L_tab$IR_upper * L_tab$weight, na.rm = TRUE) * 1000
      total_events <- sum(L_tab$events)
      total_pyrs <- sum(L_tab$pyrs)
      
      # Add to results with all columns including CIs
      results <- rbind(results, data.frame(
        ICD_CODE = icd,
        adj_IR_1k = adj_IR_1k,
        ci_lower = adj_IR_lower,
        ci_upper = adj_IR_upper,
        events = total_events,
        person_years = total_pyrs,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      log_error(paste("Error processing ICD code", icd, ":", e$message))
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
    log_message(sprintf("  %s: %.2f per 1000 PY (95%% CI: %.2f-%.2f) (%d events, %.0f person-years)", 
                       results$ICD_CODE[i], 
                       results$adj_IR_1k[i],
                       results$ci_lower[i],
                       results$ci_upper[i],
                       results$events[i],
                       results$person_years[i]))
  }
  log_message("========================================")
  log_message("Process completed successfully!")
  
  # Display results in RStudio
  cat("\n\nTop 10 Results:\n")
  print(head(results, 10))
  
}, error = function(e) {
  log_error(paste("Fatal error during calculation:", e$message))
  stop(e)
})

cat("\n\nResults saved to:", output_file, "\n")
cat("Log file saved to:", log_file, "\n")