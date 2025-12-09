DATE = format(Sys.Date(), "%Y%m%d")
OUTDIR = sprintf("/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version3/GeneratePairs/Pairs_%s", DATE)
if (!dir.exists(OUTDIR)) dir.create(OUTDIR, recursive = TRUE)

# load pair counts (ATC and ICD)
ATC = read.csv(sprintf("/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version3/GeneratePairs/ATC_pairs_%s.csv", DATE), header = TRUE)
ICD = read.csv(sprintf("/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version3/GeneratePairs/ICD_pairs_%s.csv", DATE), header = TRUE)

# remove rows with "-","-1" and "-2" in event codes 
ATC = ATC[!grepl("-|-1|-2", ATC$event_code), ]
ICD = ICD[!grepl("-|-1|-2", ICD$event_code), ]

# For ATC, only consider same-medication-pair (event_code == outcome_code)
ATC = ATC[ATC$event_code == ATC$outcome_code,]

# generate list of unique events & outcomes with prefixes
event_list1 = unique(paste0("Purch_", ATC$event_code[ATC$N_CASES_50_OUTCOMES > 500]))
outcome_list1 = unique(ATC$outcome_code[ATC$N_CASES_50_OUTCOMES > 500])
event_list2 = unique(paste0("Diag_", ICD$event_code[ICD$N_CASES_50_OUTCOMES > 500]))
outcome_list2 = unique(ICD$outcome_code[ICD$N_CASES_50_OUTCOMES > 500])

# print summary of event & outcome list lengths
cat("Number of event codes with at least 500 cases and 50 outcomes (ATC):", length(event_list1), "\n")
cat("Number of outcome codes with at least 500 cases and 50 outcomes (ATC):", length(outcome_list1), "\n")
cat("Number of event codes with at least 500 cases and 50 outcomes (ICD):", length(event_list2), "\n")
cat("Number of outcome codes with at least 500 cases and 50 outcomes (ICD):", length(outcome_list2), "\n")

# save each list to a file (no header, no quotes)
# Convert lists to data frames before exporting
event_df1 <- data.frame(event_code = event_list1)
outcome_df1 <- data.frame(outcome_code = outcome_list1)
event_df2 <- data.frame(event_code = event_list2)
outcome_df2 <- data.frame(outcome_code = outcome_list2)

write.table(event_df1, file.path(OUTDIR, sprintf("event_codes_ATC_%s.csv", DATE)), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
write.table(outcome_df1, file.path(OUTDIR, sprintf("outcome_codes_ATC_%s.csv", DATE)), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
write.table(event_df2, file.path(OUTDIR, sprintf("event_codes_ICD_%s.csv", DATE)), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
write.table(outcome_df2, file.path(OUTDIR, sprintf("outcome_codes_ICD_%s.csv", DATE)), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")

# generate CSV with successful pairs, adding prefix to event_code columns
pairs_ATC = ATC[ATC$N_CASES_50_OUTCOMES > 500, c("event_code", "outcome_code")]
pairs_ATC$event_code = paste0("Purch_", pairs_ATC$event_code)
pairs_ATC <- as.data.frame(pairs_ATC)

pairs_ICD = ICD[ICD$N_CASES_50_OUTCOMES > 500, c("event_code", "outcome_code")]
pairs_ICD$event_code = paste0("Diag_", pairs_ICD$event_code)
pairs_ICD <- as.data.frame(pairs_ICD)

# save pairs to a file (no header)
write.table(pairs_ATC, file.path(OUTDIR, sprintf("event_outcome_pairs_ATC_%s.csv", DATE)), row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(pairs_ICD, file.path(OUTDIR, sprintf("event_outcome_pairs_ICD_%s.csv", DATE)), row.names = FALSE, col.names = FALSE, quote = FALSE)