.libPaths("/shared-directory/sd-tools/apps/R/lib/")

#####################################################################
# 
# Starting from original Kela register of sickness-allowance episodes: 
# - subset to doctors of interest
# - do basic clean and QC steps
# - process benefit types
#
#####################################################################

#### Libraries
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
})

##### Arguments
TODAY <- format(Sys.Date(), "%Y%m%d")
N_THREADS <- 10
setDTthreads(N_THREADS)

# Input 
raw_kela_file       <- "/media/volume/Data_20250430/Kela/FD_2698_165_522_2023_SAIRAUSPAIVARAHA_KAUDET.csv"
doctor_list_file    <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"

# Output
out_dir             <- "/media/volume/Projects/DSGELabProject1/ProcessedData/"
log_dir             <- "/media/volume/Projects/DSGELabProject1/Logs/"

log_file            <- paste0(log_dir, "process_sickleave_data_", TODAY, ".log")
out_file            <- paste0(out_dir, "all_sickleaves_doctors_", TODAY, ".parquet")

##### Logging
log_con  <- file(log_file, open = "w")
sink(log_con, split = TRUE)
on.exit({
    try(sink(),         silent = TRUE)
    try(close(log_con), silent = TRUE)
}, add = TRUE)

cat(sprintf("=== Process Sickleaves Data ===\n"))
cat(sprintf("Start time: %s\n", Sys.time()))
cat(sprintf("Input (Kela):    %s\n", raw_kela_file))
cat(sprintf("Input (Doctor list): %s\n\n", doctor_list_file))

# ============================================================
# STEP 1: Load Doctor list
# ============================================================

doctor_ids <- fread(doctor_list_file, header = FALSE)$V1
cat(sprintf("Doctors in cohort: %d\n", length(doctor_ids)))

# ============================================================
# STEP 2: Load raw Kela file
# ============================================================

cols_of_interest <- c('FID', 'TYOKYVYTTOMYYS_ALPV', 'MAKSU_ALPV', 'MAKSU_LOPV', 'DIAGNOOSI_KOODI', 'ETUUS_KOODI', 'MAKSETTUPAIVA_LKM')

# Read DIAGNOOSI_KOODI as character so no leading zeros / formatting is lost.
sl <- fread(raw_kela_file, select = cols_of_interest, colClasses = list(character = "DIAGNOOSI_KOODI"))

# ============================================================
# STEP 3: Rename, coerce dates, basic QC
# ============================================================

setnames(sl,
    old = c('FID',       'TYOKYVYTTOMYYS_ALPV',   'MAKSU_ALPV',       'MAKSU_LOPV',     'DIAGNOOSI_KOODI', 'ETUUS_KOODI',  'MAKSETTUPAIVA_LKM'),
    new = c('DOCTOR_ID', 'DISABILITY_START_DATE', 'SICK_LEAVE_START', 'SICK_LEAVE_END', 'SICK_LEAVE_DIAG', 'BENEFIT_TYPE', 'COMPENSATED_DAYS'))

sl[, SICK_LEAVE_START      := as.IDate(SICK_LEAVE_START,      format = "%Y-%m-%d")]
sl[, SICK_LEAVE_END        := as.IDate(SICK_LEAVE_END,        format = "%Y-%m-%d")]
sl[, DISABILITY_START_DATE := as.IDate(DISABILITY_START_DATE, format = "%Y-%m-%d")]

# Remove rows with missing dates, and those where start > end. Also remove duplicates.
cat(sprintf("\nBefore QC: %d rows\n", nrow(sl)))
sl <- sl[!is.na(SICK_LEAVE_START) & !is.na(SICK_LEAVE_END)]
sl <- sl[SICK_LEAVE_START <= SICK_LEAVE_END]
sl <- unique(sl)
cat(sprintf("After QC (non-NA dates, start<=end, unique): %d rows\n", nrow(sl)))

# ============================================================
# STEP 4: Restrict to cohort doctors
# ============================================================

sl <- sl[DOCTOR_ID %in% doctor_ids]
cat(sprintf("\nAfter doctor-list restriction: %d rows\n", nrow(sl)))
cat(sprintf("Distinct doctors with >=1 sick leave: %d\n", uniqueN(sl$DOCTOR_ID)))

# ============================================================
# STEP 5: Derived columns
# ============================================================

sl[, SVA_DATE := fifelse(is.na(DISABILITY_START_DATE),
                            SICK_LEAVE_START,
                            DISABILITY_START_DATE)]

# SICK_LEAVE_DIAG is full ICD-10 code
# diag_3char / diag_first_letter  are convenience truncations for grouping.
sl[, diag_3char        := substr(SICK_LEAVE_DIAG, 1, 3)]
sl[, diag_first_letter := substr(SICK_LEAVE_DIAG, 1, 1)]

# if SICK_LEAVE_DIAG is missing, set all diagnosis columns to "Not Reported" (NA)
sl[is.na(SICK_LEAVE_DIAG) | SICK_LEAVE_DIAG == "", `:=`(
    SICK_LEAVE_DIAG     = "Not Reported",
    diag_3char          = "Not Reported",
    diag_first_letter   = "Not Reported"
)]

# ============================================================
# STEP 6: Benefit-type processing 
# ============================================================

# There are 3 types of sick leave: 73 (partial), 74 (normal), 75 (self-employed)

# The duration of sick leave and its payment depends on the benefit type. 
# Note that an additional "waiting" period between sickness/disability and the start of sick leave (payment) start may exist, mainly for self-employed people which are not covered during this period.
# Usually, DISABILITY_START_DATE indicates the start of disability, SICK_LEAVE_START indicates the start of sick leave payment. 
# For benefit type 75 (self-employed), SICK_LEAVE_START indicates the start of payed waiting period. 

# Processing rule:
# A.    If benefit type is 74 (normal), and multiple rows exist for the same DOCTOR_ID/SVA_DATE, 
#       then merge the multiple sick leave records into a single record that spans the full sick leave episode
# B.    If benefit type is 75 (self-employed), 
#       then find a record of the same doctor with benefit type 74 and with the same SVA_DATE, 
#       then merge the two records into a single record that spans the full sick leave episode

cat(sprintf("\nBefore processing benefit types: %d rows\n", nrow(sl)))

# Split by benefit type
sl73 <- sl[BENEFIT_TYPE == 73]
sl74 <- sl[BENEFIT_TYPE == 74]
sl75 <- sl[BENEFIT_TYPE == 75]

# Multiple type-74 are collapsed into a single spanning episode per key:
# - SICK_LEAVE_START = earliest start, SICK_LEAVE_END = latest end (full span)
# - COMPENSATED_DAYS = sum across the group
# - DISABILITY_START_DATE / SICK_LEAVE_DIAG / diag_3char / diag_first_letter
#   are taken from the chronologically first row (earliest SICK_LEAVE_START)
# - BENEFIT_TYPE stays "74"
sl74_ordered <- sl74[order(DOCTOR_ID, SVA_DATE, SICK_LEAVE_START)]

group_sizes <- sl74_ordered[, .N, by = .(DOCTOR_ID, SVA_DATE)]
n_multi_groups <- sum(group_sizes$N > 1)
if (n_multi_groups > 0) {
    cat(sprintf("Found %d DOCTOR_ID/SVA_DATE groups with multiple type-74 rows (%d rows total).\n", n_multi_groups, sum(group_sizes[N > 1]$N)))
}

sl74_for_match <- sl74_ordered[, .(
    DISABILITY_START_DATE = DISABILITY_START_DATE[1],
    SICK_LEAVE_START       = min(SICK_LEAVE_START),
    SICK_LEAVE_END         = max(SICK_LEAVE_END),
    SICK_LEAVE_DIAG        = SICK_LEAVE_DIAG[1],
    BENEFIT_TYPE           = "74",
    COMPENSATED_DAYS       = sum(COMPENSATED_DAYS),
    diag_3char             = diag_3char[1],
    diag_first_letter      = diag_first_letter[1]
), by = .(DOCTOR_ID, SVA_DATE)]

cat(sprintf("Type-74 rows: %d raw -> %d rows after collapsing consecutive episodes.\n", nrow(sl74), nrow(sl74_for_match)))

# Extract and match type-75 rows to their corresponding type-74 record
matched <- merge(sl75, sl74_for_match, by = c("DOCTOR_ID", "SVA_DATE"), suffixes = c("_75", "_74"))
cat(sprintf("Type-75 (self-employed) rows: %d | matched to a type-74 record: %d | unmatched: %d\n", nrow(sl75), nrow(matched), nrow(sl75) - nrow(matched)))

# Build the merged/processed record for each matched pair:
# - SICK_LEAVE_START = start of the (self-employed) waiting period, from type-75
# - SICK_LEAVE_END   = end of the payment period, from type-74
# - COMPENSATED_DAYS = sum of waiting-period days (75) + payment days (74)
# - Diagnosis / disability-start-date columns are taken from the type-74 record
# - BENEFIT_TYPE is re-coded as "75 + 74" 
sl75_processed <- data.table(
    DOCTOR_ID              = matched$DOCTOR_ID,
    DISABILITY_START_DATE  = matched$DISABILITY_START_DATE_74,
    SICK_LEAVE_START       = matched$SICK_LEAVE_START_75,
    SICK_LEAVE_END         = matched$SICK_LEAVE_END_74,
    SICK_LEAVE_DIAG        = matched$SICK_LEAVE_DIAG_74,
    BENEFIT_TYPE           = "75 + 74",
    COMPENSATED_DAYS       = matched$COMPENSATED_DAYS_75 + matched$COMPENSATED_DAYS_74,
    SVA_DATE               = matched$SVA_DATE,
    diag_3char             = matched$diag_3char_74,
    diag_first_letter      = matched$diag_first_letter_74
)

# Type-75 rows with no matching type-74 record are kept as-is (waiting period alone)
sl75_unmatched <- sl75[!matched[, .(DOCTOR_ID, SVA_DATE)], on = c("DOCTOR_ID", "SVA_DATE")]

# Remove (collapsed) type-74 rows that were merged into type-75 rows
sl74_unmatched <- sl74_for_match[!matched[, .(DOCTOR_ID, SVA_DATE)], on = c("DOCTOR_ID", "SVA_DATE")]

# Keep all benefit type 73 rows + unmatched 74 + unmatched 75 (as-is) + processed 75
sl <- rbindlist(list(sl73, sl74_unmatched, sl75_unmatched, sl75_processed), use.names = TRUE)

cat(sprintf("After processing benefit types: %d rows\n", nrow(sl)))
cat(sprintf("Benefit type breakdown:\n"))
benefit_breakdown <- sl[, .N, by = BENEFIT_TYPE][order(-N)]
benefit_breakdown[, pct := round(100 * N / sum(N), 2)]
print(benefit_breakdown)

# ============================================================
# STEP 7: Diagnostics 
# ============================================================

cat("\nDiagnosis first-letter breakdown:\n")
diag_letter_breakdown <- sl[, .N, by = diag_first_letter][order(-N)]
diag_letter_breakdown[, pct := round(100 * N / sum(N), 2)]
cat(sprintf("Total categories: %d | Showing top 10:\n", nrow(diag_letter_breakdown)))
print(head(diag_letter_breakdown, 10))

cat("\nF-code 3-char breakdown:\n")
diag_3char_breakdown <- sl[, .N, by = diag_3char][order(-N)]
diag_3char_breakdown[, pct := round(100 * N / sum(N), 2)]
cat(sprintf("Total categories: %d | Showing top 10:\n", nrow(diag_3char_breakdown)))
print(head(diag_3char_breakdown, 10))

cat("\nDiagnosis-code length distribution (raw, all chars):\n")
print(sl[, .N, by = .(code_len = nchar(SICK_LEAVE_DIAG))][order(code_len)])

cat("\nDate range:\n")
cat(sprintf("  min SICK_LEAVE_START:      %s\n", min(sl$SICK_LEAVE_START)))
cat(sprintf("  max SICK_LEAVE_END:        %s\n", max(sl$SICK_LEAVE_END)))
cat(sprintf("  min DISABILITY_START_DATE: %s\n\n", min(sl$DISABILITY_START_DATE, na.rm = TRUE)))

# ============================================================
# STEP 8: Save 
# ============================================================

write_parquet(sl, out_file)

cat(sprintf("Output: %s\n", out_file))
cat(sprintf("File size: %.1f MB\n", file.info(out_file)$size / 1024^2))
cat(sprintf("End time: %s\n", Sys.time()))

sink()
close(log_con)