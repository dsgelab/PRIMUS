# load pair counts (ATC and ICD)
DATE="20250805"
ATC = read.csv(sprintf("/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/ATC_pairs_%s.csv", DATE), header = TRUE)
ICD = read.csv(sprintf("/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/ICD_pairs_%s.csv", DATE), header = TRUE)

# remove rows with "-","-1" and "-2" in event codes 
ATC = ATC[!grepl("-|-1|-2", ATC$event_code), ]
ICD = ICD[!grepl("-|-1|-2", ICD$event_code), ]

# generate list of unique events & outcomes with prefixes
event_list1 = unique(paste0("Purch_", ATC$event_code[ATC$N_CASES_20_OUTCOMES > 500]))
outcome_list1 = unique(ATC$outcome_code[ATC$N_CASES_20_OUTCOMES > 500])
event_list2 = unique(paste0("Diag_", ICD$event_code[ICD$N_CASES_20_OUTCOMES > 500]))
outcome_list2 = unique(ICD$outcome_code[ICD$N_CASES_20_OUTCOMES > 500])

# print summary of event & outcome list lengths
cat("Number of event codes with at least 500 cases and 20 outcomes (ATC):", length(event_list1), "\n")
cat("Number of outcome codes with at least 500 cases and 20 outcomes (ATC):", length(outcome_list1), "\n")
cat("Number of event codes with at least 500 cases and 20 outcomes (ICD):", length(event_list2), "\n")
cat("Number of outcome codes with at least 500 cases and 20 outcomes (ICD):", length(outcome_list2), "\n")

# save each list to a file (no header, no quotes)
# Convert lists to data frames before exporting
event_df1 <- data.frame(event_code = event_list1)
outcome_df1 <- data.frame(outcome_code = outcome_list1)
event_df2 <- data.frame(event_code = event_list2)
outcome_df2 <- data.frame(outcome_code = outcome_list2)

write.table(event_df1, "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/event_codes_ATC.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
write.table(outcome_df1, "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/outcome_codes_ATC.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
write.table(event_df2, "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/event_codes_ICD.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
write.table(outcome_df2, "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/outcome_codes_ICD.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")

# generate CSV with successful pairs, adding prefix to event_code columns
pairs_ATC = ATC[ATC$N_CASES_20_OUTCOMES > 500, c("event_code", "outcome_code")]
pairs_ATC$event_code = paste0("Purch_", pairs_ATC$event_code)
pairs_ATC <- as.data.frame(pairs_ATC)

pairs_ICD = ICD[ICD$N_CASES_20_OUTCOMES > 500, c("event_code", "outcome_code")]
pairs_ICD$event_code = paste0("Diag_", pairs_ICD$event_code)
pairs_ICD <- as.data.frame(pairs_ICD)

# save pairs to a file (no header)
write.csv(pairs_ATC, "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/event_outcome_pairs_ATC.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
write.csv(pairs_ICD, "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/event_outcome_pairs_ICD.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")