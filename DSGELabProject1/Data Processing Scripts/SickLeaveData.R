# Libraries
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)

# Global variables
base_dir = "/media/volume/Data_20250430/Kela/"
file = "FD_2698_165_522_2023_SAIRAUSPAIVARAHA_KAUDET.csv"
cols_of_interest1 = c('FID', 'MAKSU_ALPV', 'MAKSU_LOPV', 'DIAGNOOSI_KOODI')

doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"

out_dir = "/media/volume/Projects/DSGELabProject1/ProcessedData/"
log_dir = "/media/volume/Projects/DSGELabProject1/Logs/SickLeaveData_20251013/"
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
log_file <- file.path(log_dir, "processing_log_20251013.txt")

N_THREADS = 10
setDTthreads(N_THREADS)

# MAIN

# load data
dt = fread(paste0(base_dir, file))

# filter columns of interest
dt = dt[, ..cols_of_interest1]

# translate names
setnames(dt, old = c('FID', 'MAKSU_ALPV', 'MAKSU_LOPV', 'DIAGNOOSI_KOODI'), new = c('PATIENT_ID', 'SICK_LEAVE_START', 'SICK_LEAVE_END', 'SICK_LEAVE_DIAG'))

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

# QC 1. check rows with non-DATE SICK_LEAVE_END
non_date_end <- dt[is.na(SICK_LEAVE_END), .N]
sink(log_file, append = TRUE)
cat(sprintf("Rows with non-DATE SICK_LEAVE_END: %d (%.2f%%)\n", non_date_end, 100 * non_date_end / nrow(dt)))
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
cat("Future SICK_LEAVE_END dates found:\n")
cat(paste(as.character(future_dates), collapse = ", "), "\n")
sink()

# remove duplicates
orig_nrow <- nrow(dt)
dt <- unique(dt)
removed_dups <- orig_nrow - nrow(dt)
sink(log_file, append = TRUE)
cat(sprintf("Removed %d duplicate rows (%.2f%%)\n", removed_dups, 100 * removed_dups / orig_nrow))
sink()

# calculate sick leave duration (days)
dt[, SICK_LEAVE_DURATION := as.numeric(SICK_LEAVE_END - SICK_LEAVE_START)]

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

# If general data needed, uncomment below
# fwrite(dt, file = paste0(out_dir, "SickLeaveData_ALL_20251007.csv"))

# filter only data about cohorts of doctors
doctors <- fread(doctor_list, header = FALSE)$V1
dt_doctors <- dt[PATIENT_ID %in% doctors]
n_doctors_found <- length(unique(dt_doctors$PATIENT_ID))
n_total_doctors <- length(unique(doctors))
sink(log_file, append = TRUE)
cat(sprintf("Doctors found in data: %d out of %d (%.2f%%)\n", n_doctors_found, n_total_doctors, 100 * n_doctors_found / n_total_doctors))
sink()

# If doctor data needed, uncomment below
# fwrite(dt_doctors, file = paste0(out_dir, "SickLeaveData_DOCTORS_20251007.csv"))


# Plots:
# dt = dt_doctors # uncomment to plot only doctors data

# 1. Density of sick leave start dates
p1 <- ggplot(dt, aes(x = SICK_LEAVE_START)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    labs(title = "Density of Sick Leave Start Dates", x = "Start Date", y = "Density") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Density of sick leave end dates
p2 <- ggplot(dt, aes(x = SICK_LEAVE_END)) +
    geom_density(fill = "darkorange", alpha = 0.6) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    labs(title = "Density of Sick Leave End Dates", x = "End Date", y = "Density") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Density of sick leave duration
p3 <- ggplot(dt, aes(x = SICK_LEAVE_DURATION)) +
    geom_density(fill = "forestgreen", alpha = 0.6) +
    scale_x_continuous(breaks = pretty(dt$SICK_LEAVE_DURATION, n = 10)) +
    labs(title = "Density of Sick Leave Duration", x = "Duration (days)", y = "Density") +
    theme_minimal()

# Stack plots & save
combined_plot <- p1 / p2 / p3
ggsave(filename = paste0(log_dir, "sick_leave_distributions_ALL_20251013.png"), plot = combined_plot, width = 10, height = 15)

# uncomment to save doctors-only plot
#ggsave(filename = paste0(log_dir, "sick_leave_distributions_DOCTORS_20251013.png"), plot = combined_plot, width = 10, height = 15)


# END
rm(list = ls())
gc()
