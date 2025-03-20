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
.libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(gtsummary)

doctors  <- fread("/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250311.csv")
glimpse(doctors)

doctors <- doctors %>% mutate(
    #BIRTH_DATE = format(ymd(BIRTH_DATE), "%d-%m-%Y"),
    BIRTH_DATE = ymd(BIRTH_DATE),
    AGE_in_2023 = round(2023 - year(ymd(BIRTH_DATE)) + (as.numeric(difftime(as.Date("2023-01-01"), ymd(BIRTH_DATE), units = "days")) %% 365.25) / 365.25, 2)
)

### Age distribution of doctors ###

# Plot age distribution of doctors by sex
# Filter out counts under N = 5
age_sex_counts <- doctors %>%
    group_by(AGE_in_2023, SEX) %>%
    tally() %>%
    filter(n >= 5)

# Merge back with the original data to keep only the relevant ages
filtered_doctors <- doctors %>%
    semi_join(age_sex_counts, by = c("AGE_in_2023", "SEX"))

# Plot age distribution of doctors by sex using density plot
ggplot(filtered_doctors, aes(x = AGE_in_2023, fill = factor(SEX), color = factor(SEX))) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("1" = "blue", "2" = "pink"), labels = c("1" = "Male", "2" = "Female")) +
    scale_color_manual(values = c("1" = "blue", "2" = "pink"), labels = c("1" = "Male", "2" = "Female")) +
    labs(title = "Age Distribution of Doctors by Sex",
         x = "Age in 2023",
         y = "Density",
         fill = "Sex",
         color = "Sex") +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
    )

# Plot age distribution of doctors by sex using boxplot
ggplot(filtered_doctors, aes(x = factor(SEX, labels = c("Male", "Female")), y = AGE_in_2023, fill = factor(SEX, labels = c("Male", "Female")))) +
    geom_boxplot() +
    scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
    labs(
        title = "Age Distribution of Doctors by Sex",
        x = "Sex",
        y = "Age in 2023",
        fill = "Sex"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
    )

# Plot age distribution of doctors by sex using violin plot
ggplot(filtered_doctors, aes(x = factor(SEX, labels = c("Male", "Female")), y = AGE_in_2023, fill = factor(SEX, labels = c("Male", "Female")))) +
    geom_violin(trim = FALSE) +
    scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
    labs(
        title = "Age Distribution of Doctors by Sex",
        x = "Sex",
        y = "Age in 2023",
        fill = "Sex"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
    )

# # Plot age distribution of doctors by sex
# ggplot(doctors, aes(x = AGE_in_2023, fill = factor(SEX))) +
#     geom_histogram(binwidth = 1, position = "dodge", color = "black") +
#     scale_fill_manual(values = c("1" = "blue", "2" = "pink"), labels = c("1" = "Male", "2" = "Female")) +
#     labs(title = "Age Distribution of Doctors by Sex",
#              x = "Age in 2023",
#              y = "Count",
#              fill = "Sex") +
#     theme_minimal() +
#     theme(
#         plot.title = element_text(hjust = 0.5),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 10)
#     )

### Birth year distribution of doctors ###
# Plot birth year distribution of doctors
# Calculate birth year
doctors <- doctors %>% mutate(BIRTH_YEAR = year(BIRTH_DATE))

# Plot birth year distribution of doctors by sex using density plot
birthyear_plot <- ggplot(doctors, aes(x = BIRTH_YEAR, fill = factor(SEX), color = factor(SEX))) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("1" = "blue", "2" = "pink"), labels = c("1" = "Male", "2" = "Female")) +
    scale_color_manual(values = c("1" = "blue", "2" = "pink"), labels = c("1" = "Male", "2" = "Female")) +
    labs(title = "Birth Year Distribution of Doctors by Sex",
         x = "Birth Year",
         y = "Density",
         fill = "Sex",
         color = "Sex") +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)
    )

# Plot birth year distribution of doctors by sex using boxplot
ggplot(filtered_doctors, aes(x = factor(SEX, labels = c("Male", "Female")), y = BIRTH_YEAR, fill = factor(SEX, labels = c("Male", "Female")))) +
    geom_boxplot() +
    scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
    labs(
        title = "Birth Year Distribution of Doctors by Sex",
        x = "Sex",
        y = "Birth Year",
        fill = "Sex"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
    )


### Sex distribution ###
# Pie chart showing the distribution of sexes with absolute and percentage numbers
sex_distribution <- doctors %>%
    count(SEX) %>%
    mutate(
        percentage = n / sum(n) * 100,
        label = paste0(n, "\n(", round(percentage, 1), "%)")
    )

sex_distribution_plot <- ggplot(sex_distribution, aes(x = "", y = n, fill = factor(SEX, labels = c("Male", "Female")))) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    labs(title = "Distribution of Doctors by Sex", fill = "Sex") +
    scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
    ) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6, color = "white")

sex_distribution_plot



### Practicing days of doctors ###
# Calculate practicing days of doctors
ggplot(doctors, aes(x = practicing_days)) +
    geom_histogram(binwidth = 365, fill = "skyblue", color = "black", alpha = 0.7) +
    scale_x_continuous(labels = function(x) x / 365, breaks = seq(0, max(doctors$practicing_days) / 365, by = 5)) +
    labs(
        title = "Histogram of Practicing Years of Doctors",
        x = "Practicing Years",
        y = "Count"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
    )

# Add interpreation to specialty
spec_interpretation <- read_excel("/media/volume/Projects/jg/dictionary/condensed_specialty+interpretation_dict.xlsx") %>% 
    select(-COMPRISED, -CODEVALUE)

doctors <- doctors %>% 
    left_join(spec_interpretation, by = c("LAST_SPECIALTY" = "LABEL_EN")) %>% 
    dplyr::rename(LAST_SPECIALTY_INTERP = INTERPRETATION)

# Pie chart showing number of doctors with and without specialization
spec_status_plot <- doctors %>% 
    mutate(Specialty_Status = ifelse(LAST_SPECIALTY_INTERP == "" | is.na(LAST_SPECIALTY_INTERP), "Without Specialty", "With Specialty")) %>%
    count(Specialty_Status) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    ggplot(aes(x = "", y = n, fill = Specialty_Status)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("Without Specialty" = "orange", "With Specialty" = "steelblue")) +
    labs(title = "Number of Doctors with and without Specialty", fill = "Specialty Status") +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
    ) +
    geom_text(aes(label = paste0(n, "\n(", round(percentage, 1), "%)")), position = position_stack(vjust = 0.5), size = 6, color = "white")

# Plot number of doctors per specialty (based on final specialty)
spec_count_plot <- doctors %>%
    filter(LAST_SPECIALTY != "") %>%
    group_by(LAST_SPECIALTY_INTERP) %>%
    summarise(count = n_distinct(FID)) %>%
    filter(count >= 5) %>% 
    ggplot(aes(x = reorder(LAST_SPECIALTY_INTERP, -count), y = count)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
    coord_flip() +
    labs(title = "Counts of Doctors Across Specialties", x = "Specialty", y = "Count of Doctors") +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
    ) +
    geom_text(aes(label = count), hjust = -0.2, size = 5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

library(gridExtra)
grid.arrange(birthyear_plot, sex_distribution_plot, spec_status_plot, spec_count_plot, ncol = 2, layout_matrix = rbind(c(1, 2), c(3, 4)))



### Doctors patient summary ###
dp_summary <- fread("/media/volume/Projects/DSGELabProject1/doctor_patient_summary_20250220_v2.csv") %>% 
    filter(DOCTOR_ID != "DOCTOR_ID")
glimpse(dp_summary)

# How many doctors self-prescribe/self-diagnose?
# Calculate self-prescription statistics
self_prescription_stats <- dp_summary %>%
    mutate(SelfPrescription = ifelse(SelfPrescription > 0, "SelfPrescribing", "Not SelfPrescribing")) %>%
    group_by(SelfPrescription) %>%
    summarise(
        Count = n(),
        Percentage = round((n() / nrow(dp_summary)) * 100, 2),
        Median = median(SelfPrescription),
        Q25 = quantile(SelfPrescription, 0.25),
        Q75 = quantile(SelfPrescription, 0.75),
        Mean = round(mean(SelfPrescription), 2),
        SD = round(sd(SelfPrescription), 2)
    ) %>%
    mutate(Percentage = paste0(Percentage, "%"))

# # Calculate self-diagnosis statistics
# self_diagnosis_stats <- dp_summary %>%
#     mutate(SelfEvents = ifelse(SelfDiagnosis > 0, "SelfDiagnosing", "Not SelfDiagnosing")) %>%
#     group_by(SelfEvents) %>%
#     summarise(
#         Count = n(),
#         Percentage = round((n() / nrow(dp_summary)) * 100, 2),
#         Median = median(SelfDiagnosis),
#         Q25 = quantile(SelfDiagnosis, 0.25),
#         Q75 = quantile(SelfDiagnosis, 0.75),
#         Mean = round(mean(SelfDiagnosis), 2),
#         SD = round(sd(SelfDiagnosis), 2)
#     ) %>%
#     mutate(Percentage = paste0(Percentage, "%"))

# combined_stats <- bind_rows(self_prescription_stats, self_diagnosis_stats)

# Print the table using gtsummary
self_prescription_stats %>%
    gt::gt() %>%
    gt::tab_header(
        title = "Self-Prescription Statistics (N = 30,542)"
    )



# How much do doctors work in public vs private sector as a function of practicing days?
dp_summary <- dp_summary %>% 
    left_join(doctors %>% select(FID, practicing_days), by = c("DOCTOR_ID" = "FID"))

dp_summary_pp <- dp_summary %>%
    mutate(public_practice = (PublicPrescriptions / (PrivatePrescriptions + PublicPrescriptions) / PublicPrescriptions) * 100,
        practicingYears = round(practicing_days / 365.25, 2)) %>%
        group_by(practicingYears) %>%
        summarise(mean_pct_public = mean(public_practice, na.rm = TRUE),
            sd_pct_public = sd(public_practice, na.rm = TRUE),
            n = n(),
            se = sd_pct_public / sqrt(n),
            lower_ci = mean_pct_public - 1.96 * se,
            upper_ci = mean_pct_public + 1.96 * se)

# Plotting mean percentage in public setting with 95% CI and trendline
ggplot(dp_summary_pp, aes(x = practicingYears, y = mean_pct_public)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    scale_x_continuous(limits = c(5, 50)) +
    scale_y_continuous(limits = c(60, 75), breaks = seq(99, 100, by = 1)) +
    labs(
        title = "Percentage in Public Setting by Practicing Years",
        x     = "Practicing Years",
        y     = "Mean Percentage in Public (%)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
    ) +
    annotate("text", x = 30, y = 74, label = paste("Slope:", round(coef(lm(mean_pct_public ~ practicingYears, data = dp_summary_pp))[2], 4)), color = "red")

# Randomization factor: Unique patients vs total patients per doctor
dp_summary %>% 
    mutate(r_fct = UniquePatients / TotalPatients,
    PracticingYears = round(practicing_days / 365.25, 0))  %>% 
    group_by(PracticingYears) %>%
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
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
    ) 

# sanity check: plot total number of patients vs practicing years
dp_summary %>%
    mutate(PracticingYears = round(practicing_days / 365.25, 0)) %>%
    group_by(PracticingYears) %>%
    summarise(
        mean_total_patients = mean(UniquePatients, na.rm = TRUE),
        sd_total_patients = sd(UniquePatients, na.rm = TRUE),
        n = n(),
        se = sd_total_patients / sqrt(n),
        lower_ci = mean_total_patients - 1.96 * se,
        upper_ci = mean_total_patients + 1.96 * se
    ) %>%
    ggplot(aes(x = PracticingYears, y = mean_total_patients)) +
    geom_histogram(
        data = dp_summary %>% mutate(PracticingYears = round(practicing_days / 365.25, 0)),
        aes(x = PracticingYears, y = ..count.. * 10),
        binwidth = 1, fill = "grey", alpha = 0.3
    ) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    scale_x_continuous(limits = c(5, 50)) +
    scale_y_continuous(
        name = "Total Patients",
        sec.axis = sec_axis(
            trans = ~ . / 10, # Undo the *10 used in geom_histogram
            name = "Number of Doctors (Histogram)",
            breaks = seq(0, 1000, 200)
        )
    ) +
    labs(
        title = "Total Patients by Practicing Years",
        x     = "Practicing Years",
        y     = "Total Patients"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
    )


# stratified by register
dp_summary %>% 
    pivot_longer(cols = c(Purchases, Prescriptions, DiagnosisAvohilmo, DiagnosisHilmo), names_to = "Register", values_to = "Patients") %>%
    mutate(PracticingYears = round(practicing_days / 365.25, 0)) %>% 
    group_by(PracticingYears) %>% 
    filter(n() >= 5) %>% 
    ungroup %>% 
    group_by(Register, PracticingYears) %>%
    summarise(mean_total_patients = mean(Patients, na.rm = TRUE),
        sd_total_patients = sd(Patients, na.rm = TRUE),
        n = n(),
        se = sd_total_patients / sqrt(n),
        lower_ci = mean_total_patients - 1.96 * se,
        upper_ci = mean_total_patients + 1.96 * se) %>% 
    ggplot(aes(x = PracticingYears, y = mean_total_patients)) +
    geom_line(color = "blue") +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +    
    # scale_x_continuous(limits = c(5, 50)) +
    # scale_y_continuous(limits = c(60, 75), breaks = seq(99, 100, by = 1)) +
    labs(
        title = "Total Patients by Practicing Years Stratified by Register",
        x     = "Practicing Years",
        y     = "Total Patients"
    ) +
    facet_wrap(~ Register, scales = "free_y") +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
    )


# quick check: whether some doctors are already dead in 2023 despite license being valid

dvv <- fread("/media/volume/Data/DVV/...")
dvv <- dvv %>% 
    filter(FID %in% doctors$FID) %>%
    #filter(!is.na(Kuolinpäivä)) %>%
    mutate(DeathDate = ymd(Kuolinpäivä)) %>% 
    select(FID, DeathDate)

doctors %>% 
    left_join(dvv, by = "FID") %>% 
    filter(!is.na(DeathDate)) %>%
    filter(DeathDate < end_date)

doctors %>% 
    left_join(dvv, by = "FID") %>%
    filter(practicing_days > 50*365)

# Randomization factor by specialization: Unique patients vs total patients per doctor
dp_summary %>% 
    left_join(doctors %>% select(FID, SPECIALTY = LAST_SPECIALTY_INTERP), by = c("DOCTOR_ID" = "FID")) %>% 
    mutate(SPECIALTY = ifelse(SPECIALTY == "", "Unknown", SPECIALTY),
        SPECIALTY = ifelse(SPECIALTY == "Occupational Dentistry", "Occup. Dentistry", SPECIALTY),
        r_fct = UniquePatients / TotalPatients) %>% 
    group_by(SPECIALTY) %>% 
    mutate(median_r_fct = median(r_fct, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(x = reorder(SPECIALTY, -median_r_fct), y = r_fct, fill = SPECIALTY)) +
    geom_boxplot() +
    stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -0.5, color = "black", size = 3.5) +
    geom_hline(aes(yintercept = median(r_fct, na.rm = TRUE)), linetype = "dashed", color = "red") +
    geom_text(aes(x = 1, y = median(r_fct, na.rm = TRUE), label = round(median(r_fct, na.rm = TRUE), 2)), color = "red", vjust = -1) +
    labs(
        title = "Randomization Factor by Specialty",
        x = "Specialty",
        y = "Randomization Factor (Ratio: Unique to Total Patients)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
    )
