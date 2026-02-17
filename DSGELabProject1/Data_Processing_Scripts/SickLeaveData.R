# Libraries
library(data.table)
library(ggplot2)
library(patchwork)

# Global variables
base_dir = "/media/volume/Data_20250430/Kela/"
filename = "FD_2698_165_522_2023_SAIRAUSPAIVARAHA_KAUDET.csv"
cols_of_interest = c('FID', 'TYOKYVYTTOMYYS_ALPV', 'MAKSU_ALPV', 'MAKSU_LOPV', 'DIAGNOOSI_KOODI', 'ETUUS_KOODI', 'MAKSETTUPAIVA_LKM')
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"

out_dir = "/media/volume/Projects/DSGELabProject1/ProcessedData/"
log_dir = "/media/volume/Projects/DSGELabProject1/Logs/SickLeaveData_20260216/"
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
log_file <- file.path(log_dir, "processing_log_20260216.txt")

N_THREADS = 10
setDTthreads(N_THREADS)

#-----------------------------------------
# 1. Load and clean up data
#-----------------------------------------

# load data
dt = fread(paste0(base_dir, filename))
# filter columns of interest
dt = dt[, ..cols_of_interest]

# translate names to english
setnames(dt, 
    old = c('FID','TYOKYVYTTOMYYS_ALPV', 'MAKSU_ALPV', 'MAKSU_LOPV', 'DIAGNOOSI_KOODI', 'ETUUS_KOODI', 'MAKSETTUPAIVA_LKM'), 
    new = c('PATIENT_ID', 'DISABILITY_START_DATE', 'SICK_LEAVE_START', 'SICK_LEAVE_END', 'SICK_LEAVE_DIAG', 'BENEFIT_TYPE', 'COMPENSATED_DAYS'))

# remove missing dates
orig_nrow_dt <- nrow(dt)
dt <- dt[!is.na(SICK_LEAVE_START) & !is.na(SICK_LEAVE_END)]
removed_dt <- orig_nrow_dt - nrow(dt)

sink(log_file, append = TRUE)
cat(sprintf("Rows with missing start or end date: removed %d rows (%.2f%%)\n", removed_dt, 100 * removed_dt / orig_nrow_dt))
sink()

# process dates
dt[, SICK_LEAVE_START := as.IDate(SICK_LEAVE_START, format = "%Y-%m-%d")]
dt[, SICK_LEAVE_END := as.IDate(SICK_LEAVE_END, format = "%Y-%m-%d")]

# QC 1. check rows with non-date-format SICK_LEAVE_END
non_date_end <- dt[is.na(SICK_LEAVE_END), .N]
sink(log_file, append = TRUE)
cat(sprintf("Rows with non-date-format SICK_LEAVE_END: %d (%.2f%%)\n", non_date_end, 100 * non_date_end / nrow(dt)))
sink()
dt <- dt[!is.na(SICK_LEAVE_END)]

# QC 2. Check for SICK_LEAVE_START after SICK_LEAVE_END
n_start_after_end <- dt[SICK_LEAVE_START > SICK_LEAVE_END, .N]
sink(log_file, append = TRUE)
cat(sprintf("Rows with SICK_LEAVE_START after SICK_LEAVE_END: %d (%.2f%%)\n", n_start_after_end, 100 * n_start_after_end / nrow(dt)))
sink()
dt <- dt[SICK_LEAVE_START <= SICK_LEAVE_END]

# QC 3. Check for SICK_LEAVE_END after 31-12-2023 (end of study)
n_future <- dt[SICK_LEAVE_END > as.IDate("2023-12-31"), .N]
future_rows <- dt[SICK_LEAVE_END > as.IDate("2023-12-31")]
future_dates <- sort(unique(future_rows$SICK_LEAVE_END))
sink(log_file, append = TRUE)
cat(sprintf("Rows with SICK_LEAVE_END after 31-12-2023: %d (%.2f%%)\n", n_future, 100 * n_future / nrow(dt)))
sink()

# QC 4. Check BENEFIT_TYPEs available
# expecting 3 types: 73 (partial), 74 (normal), 75 (self-employed)
benefit_types <- sort(unique(dt$BENEFIT_TYPE))
sink(log_file, append = TRUE)
cat(sprintf("Number of BENEFIT_TYPEs available: %d\n", length(benefit_types)))
cat(sprintf("BENEFIT_TYPEs found: %s\n", paste(benefit_types, collapse = ", ")))
sink()

# extract benefit type frequency
benefit_type_freq <- dt[, .N, by = BENEFIT_TYPE][order(-N)]
benefit_type_freq[, FREQ_PCT := 100 * N / sum(N)]
sink(log_file, append = TRUE)
cat("Benefit type frequency:\n")
print(benefit_type_freq)
sink()

# Remove duplicates
orig_nrow <- nrow(dt)
dt <- unique(dt)
removed_dups <- orig_nrow - nrow(dt)
sink(log_file, append = TRUE)
cat(sprintf("Removed %d duplicate rows (%.2f%%)\n", removed_dups, 100 * removed_dups / orig_nrow))
sink()

#-----------------------------------------
# 2. Calculate sick leave duration and other summaries
#-----------------------------------------

# The duration of sick leave and its payment depends on the benefit type. 
# Note that an additional "waiting" period between sickness/disability and the start of sick leave (payment) start may exist, mainly for self-employed people which are not covered during this period.
# Usually, DISABILITY_START_DATE indicates the start of disability, SICK_LEAVE_START indicates the start of sick leave payment. 
# For benefit type 75 (self-employed), SICK_LEAVE_START indicates the start of payed waiting period. 
dt = dt[BENEFIT_TYPE != 75, SICK_LEAVE_DURATION := (SICK_LEAVE_END - DISABILITY_START_DATE)]

# QC: Check for negative durations
n_negative_duration <- dt[SICK_LEAVE_DURATION < 0, .N]
sink(log_file, append = TRUE)
cat(sprintf("Rows with negative SICK_LEAVE_DURATION: %d (%.2f%%)\n", n_negative_duration, 100 * n_negative_duration / nrow(dt)))
sink()
dt <- dt[SICK_LEAVE_DURATION >= 0]

# summary of SICK_LEAVE_DURATION
duration_summary <- summary(dt$SICK_LEAVE_DURATION)
sink(log_file, append = TRUE)
cat("Summary of SICK_LEAVE_DURATION:\n")
print(duration_summary)
sink()

# summary of unique patients
n_unique_patients <- length(unique(dt$PATIENT_ID))
sink(log_file, append = TRUE)
cat(sprintf("Unique patients in data: %d\n", n_unique_patients))
sink()

#-----------------------------------------
# 3. Save results datasets
#-----------------------------------------

# filter only data about cohorts of doctors
doctors <- fread(doctor_list, header = FALSE)$V1
dt_doctors <- dt[PATIENT_ID %in% doctors]
n_doctors_found <- length(unique(dt_doctors$PATIENT_ID))
n_total_doctors <- length(unique(doctors))
sink(log_file, append = TRUE)
cat(sprintf("Doctors found in data: %d out of %d (%.2f%%)\n", n_doctors_found, n_total_doctors, 100 * n_doctors_found / n_total_doctors))
sink()

fwrite(dt_doctors, file = paste0(out_dir, "SickLeaveData_DOCTORS_20250216.csv"))

#-----------------------------------------
# 4. Plots results
#-----------------------------------------

# Histogram of sick leave duration
p <- ggplot(dt_doctors, aes(x = SICK_LEAVE_DURATION)) +
    geom_histogram(fill = "forestgreen", color = "black", alpha = 0.6, binwidth = 7) +
    labs(title = "Sick Leave Duration Distribution, 7 day bins", x = "Duration (days)", y = "Frequency") +
    theme_minimal()

# Stack plots & save
ggsave(filename = paste0(log_dir, "sick_leave_distributions_DOCTORS_20250216.png"), plot = p, width = 8, height = 6)

# END
rm(list = ls())
gc()
