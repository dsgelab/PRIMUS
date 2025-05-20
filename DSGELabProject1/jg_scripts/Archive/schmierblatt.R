# Panel A: Counts of doctors across specialties
panel_a <- valvira %>%
    group_by(LABEL_EN) %>%
    summarise(count = n_distinct(FID)) %>%
    ggplot(aes(x = reorder(LABEL_EN, -count), y = count)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Counts of Doctors Across Specialties", x = "Specialty", y = "Count of Doctors")

# Panel B: Counts of each doctor's number of specialties over their time
panel_b <- valvira %>%
    group_by(FID) %>%
    summarise(num_specialties = n_distinct(LABEL_EN)) %>%
    ggplot(aes(x = num_specialties)) +
    geom_bar() +
    labs(title = "Counts of Each Doctor's Number of Specialties", x = "Number of Specialties", y = "Count of Doctors")

# Panel C: Visualize the jumps of doctors between specialties for a subset of 10 doctors
subset_doctors <- sample(unique(valvira$FID), 10)
panel_c <- valvira %>%
    filter(FID %in% subset_doctors) %>%
    arrange(FID, START_DATE) %>%
    group_by(FID) %>%
    mutate(next_specialty = lead(LABEL_EN)) %>%
    filter(!is.na(next_specialty) & LABEL_EN != next_specialty) %>%
    ggplot(aes(x = LABEL_EN, xend = next_specialty, y = 1, yend = 1, color = as.factor(FID))) +
    geom_curve(curvature = 0.2) +
    labs(
        title = "Jumps of Doctors Between Specialties (Subset of 10 Doctors)",
        x = "Specialty",
        y = ""
    ) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Panel D: Aggregate the trajectories of all doctors
panel_d <- valvira %>%
    arrange(FID, START_DATE) %>%
    group_by(FID) %>%
    mutate(next_specialty = lead(LABEL_EN)) %>%
    filter(!is.na(next_specialty)) %>%
    group_by(LABEL_EN, next_specialty) %>%
    summarise(count = n()) %>%
    filter(count >= 5) %>%
    ggplot(aes(x = LABEL_EN, xend = next_specialty, y = count, yend = count)) +
    geom_segment(aes(size = count)) +
    labs(title = "Aggregated Jumps of Doctors Between Specialties", x = "Specialty", y = "Count of Doctors") +
    theme(legend.position = "none")

panel_d <- valvira %>%
    arrange(FID, START_DATE) %>%
    group_by(FID) %>%
    mutate(next_specialty = lead(LABEL_EN)) %>%
    filter(!is.na(next_specialty)) %>%
    group_by(LABEL_EN, next_specialty) %>%
    summarise(count = n()) %>%
    filter(count >= 5) %>%
    ggplot(aes(x = LABEL_EN, xend = next_specialty, y = count, yend = count)) +
    geom_segment(aes(size = count), color = "steelblue") +
    labs(title = "Aggregated Jumps of Doctors Between Specialties", x = "Specialty", y = "Count of Doctors", size = "Count") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

panel_d <- valvira %>%
    arrange(FID, START_DATE) %>%
    group_by(FID) %>%
    mutate(next_specialty = lead(LABEL_EN)) %>%
    filter(!is.na(next_specialty)) %>%
    group_by(LABEL_EN, next_specialty) %>%
    summarise(count = n()) %>%
    filter(count >= 5) %>%
    ggplot(aes(x = LABEL_EN, xend = next_specialty, y = count, yend = count)) +
    geom_segment(aes(size = count), color = "steelblue", arrow = arrow(length = unit(0.2, "cm"))) +
    labs(title = "Aggregated Jumps of Doctors Between Specialties", x = "Specialty", y = "Count of Doctors", size = "Count") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))



# Randomization factor by specialization: Unique patients vs total patients per doctor
dp_summary %>% 
    left_join(doctors %>% select(FID, SPECIALTY = LAST_SPECIALTY_INTERP), by = c("DOCTOR_ID" = "FID")) %>%
    mutate(r_fct = UniquePatients / TotalPatients,
    PracticingYears = round(practicing_days / 365.25, 0))  %>% 
    group_by(SPECIALTY, PracticingYears) %>%
    summarise(mean_r_fct = mean(r_fct, na.rm = TRUE),
        sd_r_fct = sd(r_fct, na.rm = TRUE),
        n = n(),
        se = sd_r_fct / sqrt(n),
        lower_ci = mean_r_fct - 1.96 * se,
        upper_ci = mean_r_fct + 1.96 * se) %>% 
    ggplot(aes(x = PracticingYears, y = mean_r_fct)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    scale_x_continuous(limits = c(5, 50)) +
    # scale_y_continuous(limits = c(60, 75), breaks = seq(99, 100, by = 1)) +
    labs(
        title = "Randomization Factor by Practicing Years",
        x     = "Practicing Years",
        y     = "Randomization Factor (Ratio: Unique to Total Patients)"
    ) +
    facet_wrap(~ SPECIALTY, scales = "free_y") +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
    ) 



echo "[$(date)] Extracting first CODE per (DOCTOR_ID, PATIENT_ID, REGISTER), sorting by DATE..." | tee -a "$LOG_FILE"

# Extract header
zcat "$INPUT_FILE" | head -n 1 > header.txt

# Find column positions dynamically
HEADER_LINE=$(zcat "$INPUT_FILE" | head -n 1)
DATE_COL=$(echo "$HEADER_LINE" | awk -F',' '{for (i=1; i<=NF; i++) if ($i == "DATE") print i}')
DOCTOR_COL=$(echo "$HEADER_LINE" | awk -F',' '{for (i=1; i<=NF; i++) if ($i == "DOCTOR_ID") print i}')
PATIENT_COL=$(echo "$HEADER_LINE" | awk -F',' '{for (i=1; i<=NF; i++) if ($i == "PATIENT_ID") print i}')
REGISTER_COL=$(echo "$HEADER_LINE" | awk -F',' '{for (i=1; i<=NF; i++) if ($i == "REGISTER") print i}')
CODE_COL=$(echo "$HEADER_LINE" | awk -F',' '{for (i=1; i<=NF; i++) if ($i == "CODE") print i}')

# Check if all required columns were found
if [[ -z "$DATE_COL" || -z "$DOCTOR_COL" || -z "$PATIENT_COL" || -z "$REGISTER_COL" || -z "$CODE_COL" ]]; then
    echo "[$(date)] Error: One or more required columns not found!" | tee -a "$LOG_FILE"
    exit 1
fi

echo "[$(date)] Found columns: DATE=$DATE_COL, DOCTOR_ID=$DOCTOR_COL, PATIENT_ID=$PATIENT_COL, REGISTER=$REGISTER_COL, CODE=$CODE_COL" | tee -a "$LOG_FILE"

# Step 1: Sort the file by DATE while preserving the header
(zcat "$INPUT_FILE" | tail -n +2 | sort -t ',' -k"$DATE_COL","$DATE_COL") > "$TEMP_SORTED_FILE"

echo "[$(date)] Sorted File: temporarily written to $TEMP_SORTED_FILE" | tee -a "$LOG_FILE"

# Step 2: Extract first occurrence of CODE grouped by (DOCTOR_ID, PATIENT_ID, REGISTER)
awk -F',' -v doc_col="$DOCTOR_COL" -v pat_col="$PATIENT_COL" -v reg_col="$REGISTER_COL" -v code_col="$CODE_COL" '
    BEGIN { OFS="," }
    NR==1 { next }  # Skip header
    {
        key = $doc_col "," $pat_col "," $reg_col;
        if (!(key in seen)) {
            seen[key] = $code_col;
            print $0;
        }
    }
' "$TEMP_SORTED_FILE" | cat header.txt - | gzip > "$OUTPUT_FILE"

# Cleanup
rm header.txt "$TEMP_SORTED_FILE"








# from first_purchases.R
mpath <- "/media/volume/Projects/DSGELabProject1/"
setwd(mpath)

dp_longitudinal <- fread("doctor_patient_longitudinal_20250220.csv.gz")
glimpse(dp_longitudinal)
head(dp_longitudinal)
table(dp_longitudinal$REGISTER) # only value "PURCHASE"
any(!is.na(dp_longitudinal$DATE))
sum(is.na(dp_longitudinal$DATE)) / nrow(dp_longitudinal) * 100  # 4.809561%
summary(dp_longitudinal$DATE)




# Extract rows from INPUT_FILE_1 and INPUT_FILE_2 based on PATIENT_IDs
echo "Extracting rows based on PATIENT_IDs..."
awk '
BEGIN {FS=OFS="\t"}
NR==FNR {ids[$1]; next}
FNR==1 {
    for (i=1; i<=NF; i++) {
        if ($i == "PATIENT_ID") patient_id_col = i
    }
    print $0
    next
}
$patient_id_col in ids
' "$PATIENT_IDS_FILE" "$INPUT_FILE_1" > temp_file_1

awk '
BEGIN {FS=OFS="\t"}
NR==FNR {ids[$1]; next}
FNR==1 {
    for (i=1; i<=NF; i++) {
        if ($i == "PATIENT_ID") patient_id_col = i
    }
    print $0
    next
}
$patient_id_col in ids
' "$PATIENT_IDS_FILE" "$INPUT_FILE_2" > temp_file_2



awk -F',' '
BEGIN {OFS=","}
FNR==1 {
    if (NR == FNR) {
        for (i=1; i<=NF; i++) header1[i] = $i
    } else {
        for (i=1; i<=NF; i++) header2[i] = $i
    }
    next
}
NR==FNR {
    file1[FNR] = $0
    next
}
{
    split(file1[FNR], row1, FS)
    output = ""
    for (i=1; i<=length(header1); i++) {
        output = output (output ? FS : "") (header1[i] in header2 ? $i : "NA")
    }
    for (i=1; i<=length(header2); i++) {
        if (!(header2[i] in header1)) {
            output = output FS $i
        }
    }
    print output
}' temp_file_1_with_source temp_file_2_with_source > "$OUTPUT_FILE"


# Example usage:
plot_patient_code(dp_prescpurch, "XXXXXXX", "A02BC01")

plot_all_codes_for_patient <- function(data, patient_id) {
    tmp_A <- data %>% 
        filter(PATIENT_ID == patient_id) %>% 
        arrange(CODE, DATE)
    
    unique_codes <- unique(tmp_A$CODE)
    
    plots <- map(unique_codes, function(code) {
        plot_A <- tmp_A %>% 
            filter(CODE == code) %>%
            ggplot(aes(x = DATE, y = Source, color = Source)) +
            geom_point() +
            scale_y_discrete(limits = c("Prescription", "Purchase")) +
            scale_x_date(limits = as.Date(c("2014-01-01", "2023-01-01"))) + # Limit x-axis
            labs(
                title = paste("Timeline of Prescriptions and Purchases (", patient_id, ", ATC = ", code, ")", sep = ""),
                x = "Date",
                y = "Register"
            ) +
            theme_minimal() +
            theme(legend.position = "none")
        
        plot_B <- tmp_A %>%
            filter(CODE == code & Source == "Purchase") %>%
            ggplot(aes(x = PRESCRIPTION_DATE, y = Source)) +
            geom_point(color = "green") +
            scale_y_discrete(
                limits = c("Purchase"),
                labels = c("Purchase" = "Imputed Prescription")
            ) +
            scale_x_date(
                limits = as.Date(c("2014-01-01", "2023-01-01"))
            ) + # Limit x-axis
            labs(
                title = paste("Timeline of Imputed Prescriptions from Purchases (", patient_id, ", ATC = ", code, ")", sep = ""),
                x = "Date",
                y = "Register"
            ) +
            theme_minimal() +
            theme(legend.position = "none")
        
        plot_A / plot_B
    })
    
    wrap_plots(plots, ncol = 2)
}

# Example usage:
plot_all_codes_for_patient(dp_prescpurch, "XXXXXXX")





####### 2) GENERATE INTERMEDIATE CSV #######
log "Generating intermediate CSV (unpipped)…"
{
  # 2a) Print new header
  printf 'PATIENT_ID,DOCTOR_ID,CODE,PRESCRIPTION_DATE,PURCHASE_DATE,FD_HASH_CODE\n'
  # 2b) Dedupe & reorder
  tail -n +2 "$INPUT_FILE" | awk -F',' -v OFS=',' \
      -v ip="$ip" -v did="$did" -v ic="$ic" \
      -v ipd="$ipd" -v iup="$iup" -v ih="$ih" '
    {
      key = $ip FS $ic FS $ipd
      if (!seen[key]++) {
        print $ip, $did, $ic, $ipd, $iup, $ih
      }
    }
  '
} > "$TMP_CSV"

log "Wrote intermediate CSV → $TMP_CSV"