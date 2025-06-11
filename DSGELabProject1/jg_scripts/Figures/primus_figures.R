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
.libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(gtsummary)
library(gt)
library(grid)

# Plot directory
plot_dir <- "/path/to/plots"

# Create timestamp for file names
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

#===============================================================================
# Figures: General Demographics of Doctors
#===============================================================================

# Load the data
doctor_IDs <- fread("/path/to/doctor_IDs.csv", header = F)
doctor_characteristics <- fread("/path/to/doctor_characteristics.csv")

# Preprocess the data
doctor_characteristics <- doctor_characteristics %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(
        BIRTH_YEAR = as.numeric(year(as.Date(BIRTH_DATE))),
        BIRTH_YEAR = ifelse(BIRTH_YEAR < 1900 | BIRTH_YEAR > 2023, NA, BIRTH_YEAR)
    )

sex_distribution <- doctor_characteristics %>%
    count(SEX) %>%
    mutate(
        percentage = n / sum(n) * 100,
        label = paste0(n, "\n(", round(percentage, 1), "%)")
    )

# Plot: What is the birth year distribution of doctors in the dataset?
# Plot birth year distribution of doctors by sex using density plot
birthyear_plot <- ggplot(doctor_characteristics, aes(x = BIRTH_YEAR, fill = factor(SEX), color = factor(SEX))) +
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

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("birthyear_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
birthyear_plot
dev.off()

# Plot: What is the sex distribution of doctors?
# Plot a pie chart of the sex distribution of doctors (same colors for sexes)
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

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("sex_distribution_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8 
)
sex_distribution_plot
dev.off()



# Plot: What proportion of doctors have a specialty?
# Plot a pie chart of proportion of doctors with vs without specialty 
spec_status_plot <- doctor_characteristics %>% 
    mutate(Specialty_Status = ifelse(INTERPRETATION == "" | is.na(INTERPRETATION), "Without Specialty", "With Specialty")) %>%
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

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("spec_status_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
spec_status_plot
dev.off()



# Plot: How are doctors distributed across different specialties
# Plot a horizontal bar chart with the number of doctors in each specialty (bars and numbers at the end of the bars)
spec_distribution_plot <- doctor_characteristics %>%
    count(INTERPRETATION) %>%
    arrange(desc(n)) %>%  # Sort by count descending
    ggplot(aes(y = factor(INTERPRETATION, levels = INTERPRETATION), x = n)) +  # Preserve the descending order
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = n), hjust = -0.2) +  # Adjust text position for horizontal bars
    labs(title = "Counts of Doctors across Specialties") +
    theme_minimal() + 
    theme(
        panel.grid = element_blank(),  # Remove grid
        axis.text.y = element_text(size = 12),  # Adjust specialty labels
        axis.text.x = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold")
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))  # Adjust x-axis to accommodate labels

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("spec_distribution_plot_", timestamp, ".pdf")),
    width = 12,  # Increased width to accommodate specialty names
    height = 8
)
spec_distribution_plot
dev.off()


# Save the General Demographics of Doctors in one figure
pdf(
    file = file.path(plot_dir, paste0("doctors_general_demographics_", timestamp, ".pdf")),
    width = 14.5,  # Increased width to accommodate specialty names
    height = 13
)
grid.arrange(birthyear_plot, sex_distribution_plot, spec_status_plot, spec_distribution_plot, ncol = 2, layout_matrix = rbind(c(1, 2), c(3, 4)))
dev.off()



#===============================================================================
# Figures: Doctors’ Family Dynamics
#===============================================================================
# Load the data
doctor_IDs <- fread("/path/to/doctor_IDs.csv", header = F)
doctor_characteristics <- fread("/path/to/doctor_characteristics.csv")
doctors_family <- fread("/path/to/doctors_and_relative.csv")

# Preprocess the data
doctor_characteristics <- doctor_characteristics %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(
        BIRTH_YEAR = as.numeric(year(as.Date(BIRTH_DATE))),
        BIRTH_YEAR = ifelse(BIRTH_YEAR < 1900 | BIRTH_YEAR > 2023, NA, BIRTH_YEAR)
    )

N_doctors = nrow(doctor_IDs)


# Plot: What proportion of doctors are married?
# A doctor is considered married if they have at least one 'SPOUSE' in RELATIVE_TYPE
married_status <- doctors_family %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    group_by(DOCTOR_ID) %>%
    summarize(is_married = any(RELATIVE_TYPE == "SPOUSE")) %>%
    ungroup() %>%
    mutate(Marital_Status = ifelse(is_married, "Married", "Not Married")) %>%
    count(Marital_Status) %>% # add the number of IDs who are not in the relative df to not married and adjust N
    mutate(percentage = n / N_doctors * 100)

married_status_plot <- ggplot(married_status, aes(x = "", y = n, fill = Marital_Status)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("Married" = "#cf1eb4", "Not Married" = "gray70")) +
    labs(title = "Proportion of Doctors Who Are Married", fill = "Marital Status") +
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
    geom_text(aes(label = paste0(n, "\n(", round(percentage, 1), "%)")), 
              position = position_stack(vjust = 0.5), size = 6, color = "white")

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("married_status_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
married_status_plot
dev.off()


# Plot: What proportion of doctors are married to other doctors? And which specialties tend to marry to each other? (relative %)
# Identify doctors who are married to other doctors
# Lookup table for doctor_id -> specialty (remove empty specialties)
doctor_specialty <- doctor_characteristics %>%
    filter(!is.na(INTERPRETATION) & INTERPRETATION != "") %>%
    select(DOCTOR_ID, INTERPRETATION)

# Find marriages where both partners are doctors with a non-empty specialty
doctor_spouses <- doctors_family %>%
    filter(RELATIVE_TYPE == "SPOUSE", DOCTOR_ID %in% doctor_specialty$DOCTOR_ID, RELATIVE_ID %in% doctor_specialty$DOCTOR_ID)

# Join to get both partners' specialties
doctor_spouses <- doctor_spouses %>%
    left_join(doctor_specialty, by = c("DOCTOR_ID" = "DOCTOR_ID")) %>%
    left_join(doctor_specialty, by = c("RELATIVE_ID" = "DOCTOR_ID"), suffix = c("_DOCTOR", "_SPOUSE"))

# Only keep marriages within the same specialty
same_spec_marriages <- doctor_spouses %>%
    filter(INTERPRETATION_DOCTOR == INTERPRETATION_SPOUSE) %>%
    distinct(DOCTOR_ID, RELATIVE_ID, INTERPRETATION_DOCTOR)

# Count number of such marriages per specialty (divide by 2 to avoid double-counting)
marriages_per_spec <- same_spec_marriages %>%
    group_by(INTERPRETATION_DOCTOR) %>%
    summarize(n_marriages = n() / 2) %>%
    ungroup()

# Number of doctors per specialty
doctors_per_spec <- doctor_specialty %>%
    group_by(INTERPRETATION) %>%
    summarize(n_doctors = n()) %>%
    ungroup()

# Merge and calculate percentage
spec_marriage_stats <- marriages_per_spec %>%
    left_join(doctors_per_spec, by = c("INTERPRETATION_DOCTOR" = "INTERPRETATION")) %>%
    mutate(perc_married = 100 * n_marriages / n_doctors) %>% 
    filter(n_marriages >= 5) # n >= 5 for data protection reasons

# Plot: Number of marriages within each specialty (bar plot)
spec_marriage_count_plot <- ggplot(spec_marriage_stats, aes(x = reorder(INTERPRETATION_DOCTOR, n_marriages), y = n_marriages)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round(n_marriages, 1)), hjust = -0.1, size = 4) +
    coord_flip() +
    labs(title = "Number of Marriages Within Each Specialty",
         x = "Specialty", y = "Number of Marriages") +
    theme_minimal() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Plot: Percentage of doctors married to another doctor in same specialty (bar plot)
spec_marriage_perc_plot <- ggplot(spec_marriage_stats, aes(x = reorder(INTERPRETATION_DOCTOR, perc_married), y = perc_married)) +
    geom_bar(stat = "identity", fill = "darkorchid") +
    geom_text(aes(label = paste0(round(perc_married, 1), "%")), hjust = -0.1, size = 4) +
    coord_flip() +
    labs(title = "Percentage of Doctors Married to Another Doctor in Same Specialty",
         x = "Specialty", y = "Percentage (%)") +
    theme_minimal() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Save the plots as pdf files
pdf(
    file = file.path(plot_dir, paste0("spec_marriage_count_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
spec_marriage_count_plot
dev.off()

pdf(
    file = file.path(plot_dir, paste0("spec_marriage_perc_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)  
spec_marriage_perc_plot
dev.off()

# New: Pie chart of marriage types (within specialty, across specialty, non-doctor/no specialty)
# For each doctor, determine marriage type
doctor_marriage_type <- doctors_family %>%
    filter(RELATIVE_TYPE == "SPOUSE", DOCTOR_ID %in% doctor_IDs$V1) %>%
    left_join(doctor_specialty, by = c("DOCTOR_ID" = "DOCTOR_ID")) %>%
    left_join(doctor_specialty, by = c("RELATIVE_ID" = "DOCTOR_ID"), suffix = c("_DOCTOR", "_SPOUSE")) %>%
    mutate(
        marriage_type = case_when(
            !is.na(INTERPRETATION_DOCTOR) & !is.na(INTERPRETATION_SPOUSE) & INTERPRETATION_DOCTOR == INTERPRETATION_SPOUSE ~ "Within Specialty",
            !is.na(INTERPRETATION_DOCTOR) & !is.na(INTERPRETATION_SPOUSE) & INTERPRETATION_DOCTOR != INTERPRETATION_SPOUSE ~ "Across Specialty",
            TRUE ~ "Non-doctor/No Specialty"
        )
    ) %>%
    select(DOCTOR_ID, marriage_type) %>%
    distinct(DOCTOR_ID, .keep_all = TRUE)

# For doctors with no spouse, add "No Spouse" (optional, but not requested)
# For now, restrict to doctors with a spouse
marriage_type_dist <- doctor_marriage_type %>%
    count(marriage_type) %>%
    mutate(perc = n / sum(n) * 100,
           label = paste0(n, "\n(", round(perc, 1), "%)"))

marriage_type_pie_plot <- ggplot(marriage_type_dist, aes(x = "", y = n, fill = marriage_type)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    scale_fill_manual(values = c("Within Specialty" = "#1b9e77", "Across Specialty" = "#7570b3", "Non-doctor/No Specialty" = "#d95f02")) +
    labs(title = "Marriage Types Among Doctors with a Spouse", fill = "Marriage Type") +
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

# Save the marriage type pie chart as a pdf file
pdf(
    file = file.path(plot_dir, paste0("marriage_type_pie_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)   
marriage_type_pie_plot
dev.off()

# Save the Doctors' Family Dynamics in one figure
pdf(
    file = file.path(plot_dir, paste0("doctors_family_dynamics_", timestamp, ".pdf")),
    width = 14.5,
    height = 8
)
grid.arrange(married_status_plot, spec_marriage_count_plot, spec_marriage_perc_plot, ncol = 3)
dev.off()


# Plot: What proportion of doctors have children? (restrict to Doctors with birth year ≤ 1980) 
# Plot: What is the average number of children per doctor by specialty?
# Filter doctors born <= 1980
doctors_1980 <- doctor_characteristics %>% filter(BIRTH_YEAR <= 1980)

# Children per doctor
children_counts <- doctors_family %>%
    filter(DOCTOR_ID %in% doctors_1980$DOCTOR_ID, RELATIVE_TYPE == "CHILD") %>%
    count(DOCTOR_ID, name = "n_children")

# Merge with doctors (ensure all doctors included, even those with 0 children)
doctors_1980 <- doctors_1980 %>%
    left_join(children_counts, by = "DOCTOR_ID") %>%
    mutate(n_children = replace_na(n_children, 0))

# Proportion with children
prop_with_children <- doctors_1980 %>%
    mutate(has_child = n_children > 0) %>%
    count(has_child) %>%
    mutate(perc = n / sum(n) * 100,
                 label = paste0(n, "\n(", round(perc, 1), "%)"))

# Pie chart: Proportion with children
prop_with_children_plot <- ggplot(prop_with_children, aes(x = "", y = n, fill = has_child)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "orange"),
                                        labels = c("FALSE" = "No Children", "TRUE" = "Has Children")) +
    labs(title = "Proportion of Doctors (Born ≤1980) With Children", fill = "Has Children") +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                panel.grid = element_blank(), plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 6, color = "white")

# Bar chart: Average number of children per specialty with 95% CI
avg_children_spec <- doctors_1980 %>%
    filter(!is.na(INTERPRETATION) & INTERPRETATION != "") %>%
    group_by(INTERPRETATION) %>%
    summarize(
        avg_children = mean(n_children),
        n = n(),
        sd_children = sd(n_children),
        se = sd_children / sqrt(n),
        ci_lower = avg_children - qt(0.975, df = n - 1) * se,
        ci_upper = avg_children + qt(0.975, df = n - 1) * se
    ) %>%
    filter(n >= 10) %>% # Only show specialties with at least 10 doctors for privacy
    arrange(avg_children)

# Calculate overall average number of children across all specialties
overall_avg_children <- mean(avg_children_spec$avg_children)

avg_children_spec_plot <- ggplot(avg_children_spec, aes(x = avg_children, y = reorder(INTERPRETATION, avg_children))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.3, color = "black") +
    geom_text(aes(label = round(avg_children, 2)), hjust = -0.1, size = 4) +
    geom_vline(xintercept = overall_avg_children, linetype = "dashed", color = "red", size = 1) +
    annotate("text", 
             x = overall_avg_children + 0.15 * diff(range(avg_children_spec$avg_children)), 
             y = 1, 
             label = paste0("Overall Avg: ", round(overall_avg_children, 2)),
             color = "red", hjust = 0, vjust = -1, size = 4, angle = 90) +
    labs(title = "Average Number of Children per Doctor by Specialty (Born ≤1980)",
         x = "Average Number of Children (95% CI)", y = "Specialty") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))

# Save the plots as pdf files
pdf(
    file = file.path(plot_dir, paste0("prop_with_children_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
prop_with_children_plot
dev.off()

pdf(
    file = file.path(plot_dir, paste0("avg_children_spec_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
avg_children_spec_plot
dev.off()

# Save the Doctors' Children Dynamics in one figure
pdf(
    file = file.path(plot_dir, paste0("doctors_children_dynamics_", timestamp, ".pdf")),
    width = 14.5,
    height = 8
)
grid.arrange(prop_with_children_plot, avg_children_spec_plot, ncol = 2)
dev.off()

#===============================================================================
# Figures: Comparison of Doctors vs General Population
#===============================================================================

# Load the data
doctor_IDs <- fread("/path/to/doctor_IDs.csv", header = F)
doctor_characteristics <- fread("/path/to/doctor_characteristics.csv")
dvv <- fread("/path/to/dvv.csv")

# Preprocess the data
doctor_characteristics <- doctor_characteristics %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(
        BIRTH_YEAR = as.numeric(year(as.Date(BIRTH_DATE))),
        BIRTH_YEAR = ifelse(BIRTH_YEAR < 1900 | BIRTH_YEAR > 2023, NA, BIRTH_YEAR)
    )

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
        age = round(as.numeric(difftime(
            dplyr::if_else(is.na(DEATH_DATE), as.Date("2023-12-31"), DEATH_DATE),
            BIRTH_DATE,
            units = "days")) / 365.25, 2),
        DEATH = ifelse(is.na(DEATH_DATE), 0, 1)
    )

# Match doctors with general population by age and sex
set.seed(123)

# 1) Compute how many doctors vs. non‐docs in each (SEX, age) cell,
#    then keep only strata where both groups exist and record n_match = min(#doc, #non).
counts <- dvv %>%
  group_by(SEX, age) %>%
  summarize(
    n_doc   = sum(is_doctor == 1),
    n_non   = sum(is_doctor == 0),
    n_match = min(n_doc, n_non),
    .groups = "drop"
  ) %>%
  filter(n_match > 0) %>%
  select(SEX, age, n_match)

# 2) Restrict to rows belonging to those “matchable” strata
dvv2 <- dvv %>%
  inner_join(counts, by = c("SEX", "age"))

# 3) From each (SEX, age), randomly pick exactly n_match doctors
matched_docs <- dvv2 %>%
  filter(is_doctor == 1) %>%
  group_by(SEX, age) %>%
  mutate(rn = sample(seq_len(n()), n())) %>%
  ungroup() %>%
  filter(rn <= n_match)

# 4) From each (SEX, age), randomly pick exactly n_match non‐doctors
matched_nondocs <- dvv2 %>%
  filter(is_doctor == 0) %>%
  group_by(SEX, age) %>%
  mutate(rn = sample(seq_len(n()), n())) %>%
  ungroup() %>%
  filter(rn <= n_match)

# 5) Stack them together and drop helper columns
matched_df <- bind_rows(matched_docs, matched_nondocs) %>%
  select(-rn, -n_match)

# matched_df now has 1:1 matched doctors vs. non‐doctors, 
# exactly balanced on SEX & age.
# You can verify:
table(matched_df$is_doctor)             # equal counts of 0 vs. 1
with(matched_df, table(SEX, is_doctor))  # same distribution by stratum

ggplot(matched_df, aes(x = age, fill = factor(is_doctor, labels = c("Not Doctor", "Doctor")))) +
    geom_histogram(binwidth = 1, position = "dodge", color = "black") +
    facet_wrap(~ factor(SEX, labels = c("Male", "Female"))) +
    scale_fill_manual(values = c("Not Doctor" = "gray70", "Doctor" = "steelblue")) +
    labs(title = "Age Distribution: Doctors vs Not Doctors by Sex",
         x = "Age",
         y = "Count",
         fill = "Group") +
    theme_minimal()


# Run Cox proportional hazards model of survival time until outcome DEATH on doctor status
cox_model <- coxph(Surv(age, DEATH) ~ is_doctor, data = matched_df)
summary(cox_model)

# Fit Kaplan-Meier
km_fit <- survfit(Surv(age, DEATH) ~ is_doctor, data = matched_df)

# Convert survfit object to data frame for ggplot2 (with CI)
km_df <- data.frame(
    time = km_fit$time,
    surv = km_fit$surv,
    lower = km_fit$lower,
    upper = km_fit$upper,
    Group = factor(rep(names(km_fit$strata), km_fit$strata), labels = c("Not Doctor", "Doctor"))
)

# Plot with ggplot2, including confidence intervals
km_plot <- ggplot(km_df, aes(x = time, y = surv, color = Group, fill = Group)) +
    geom_step(size = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    labs(
        title = "Kaplan-Meier Survival Curves: Doctors vs Not Doctors",
        x = "Age",
        y = "Survival Probability",
        color = "Group",
        fill = "Group"
    ) +
    scale_color_manual(values = c("gray70", "steelblue")) +
    scale_fill_manual(values = c("gray70", "steelblue")) +
    theme_minimal(base_size = 15)


# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("km_plot_doctor_vs_nondoctor_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(km_plot)
dev.off()


#===============================================================================
# Figures: Prescriptions / Non-Redeemed Medications
#===============================================================================
# Load the data
doctor_IDs <- fread("/path/to/doctor_IDs.csv", header = F)
doctor_characteristics <- fread("/path/to/doctor_characteristics.csv")
prescription_counts <- fread("/path/to/doctor_prescriptions_by_year.csv") # columns: DOCTOR_ID, YEAR, COUNT

# Preprocess the data
prescription_counts <- prescription_counts %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(YEAR = as.numeric(YEAR)) 

# Calculate average and 95% CI per year
prescription_summary <- prescription_counts %>%
    filter(YEAR >= 1998, YEAR <= 2022)  %>% # Filter years 1998-2022
    group_by(YEAR) %>%
    summarize(
        avg_count = mean(COUNT, na.rm = TRUE),
        n = n(),
        sd_count = sd(COUNT, na.rm = TRUE),
        se = sd_count / sqrt(n),
        ci_lower = avg_count - qt(0.975, df = n - 1) * se,
        ci_upper = avg_count + qt(0.975, df = n - 1) * se
    )

# Plot
prescription_trend_plot <- ggplot(prescription_summary, aes(x = YEAR, y = avg_count)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "steelblue", alpha = 0.2) +
    geom_point(size = 2, color = "steelblue") +
    labs(
        title = "Average Prescription Counts per Doctor (1998-2022)",
        x = "Year",
        y = "Average Prescription Count"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
    )

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("avg_prescription_counts_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(prescription_trend_plot)
dev.off()


# Plot: What is the distribution (median and range) of prescription counts per doctor by year? 
# Add yearly boxplots of prescription counts
prescription_counts_filtered <- prescription_counts %>%
    filter(YEAR >= 1998, YEAR <= 2022)

# Compute number of doctors per year for coloring
doctor_counts_per_year <- prescription_counts_filtered %>%
    group_by(YEAR) %>%
    summarize(n_doctors = n())

# Merge doctor counts into the prescription data
prescription_counts_filtered <- prescription_counts_filtered %>%
    left_join(doctor_counts_per_year, by = "YEAR")

# Create a blue gradient palette based on the number of doctors
# Add count number above the median line for each year
# Precompute median and count per year for annotation
boxplot_labels <- prescription_counts_filtered %>%
    group_by(YEAR) %>%
    summarize(
        median_y = median(COUNT, na.rm = TRUE),
        count = n()
    )

prescription_boxplot <- ggplot(prescription_counts_filtered, aes(x = factor(YEAR), y = COUNT, fill = n_doctors)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
    scale_fill_gradient(low = "#cce6ff", high = "#084594", name = "Number of Doctors") +
    labs(
        title = "Distribution of Prescription Counts per Doctor by Year",
        x = "Year",
        y = "Prescription Count"
    ) +
    coord_cartesian(ylim = c(0, 10000)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1)
    ) +
    geom_text(
        data = boxplot_labels,
        aes(x = factor(YEAR), y = median_y + 500, label = count),
        color = "black",
        size = 4,
        inherit.aes = FALSE
    )


# Save the boxplot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("prescription_counts_boxplot_", timestamp, ".pdf")),
    width = 14,
    height = 8
)
print(prescription_boxplot)
dev.off()


# Plot: What is the distribution of prescription counts per doctor by year by specialty?
# Add specialty information to prescription counts
prescription_counts_with_spec <- prescription_counts_filtered %>%
    left_join(doctor_characteristics %>% select(DOCTOR_ID, INTERPRETATION), by = "DOCTOR_ID") %>%
    rename(SPECIALTY = INTERPRETATION) %>%
    # filter(!is.na(SPECIALTY) & SPECIALTY != "") %>% 
    mutate(SPECIALTY = ifelse(is.na(SPECIALTY) | SPECIALTY == "", "NO SPECIALTY", SPECIALTY)) 

# Find top 10 specialties by total doctor count (excluding "NO SPECIALTY")
top10_specialties <- doctor_characteristics %>%
    mutate(SPECIALTY = ifelse(is.na(INTERPRETATION) | INTERPRETATION == "", "NO SPECIALTY", INTERPRETATION)) %>%
    count(SPECIALTY, sort = TRUE) %>%
    slice_head(n = 10) %>%
    pull(SPECIALTY)

# Filter prescription data to only top 10 specialties (including "NO SPECIALTY")
prescription_counts_top10_spec <- prescription_counts_with_spec %>%
    filter(SPECIALTY %in% top10_specialties)

# Create a boxplot for each of the top 10 specialties by year
prescription_boxplot_by_spec <- ggplot(prescription_counts_top10_spec, aes(x = factor(YEAR), y = COUNT, fill = SPECIALTY)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
    scale_fill_brewer(palette = "Set3") +
    labs(
        title = "Distribution of Prescription Counts per Doctor by Year (Top 10 Specialties)",
        x = "Year",
        y = "Prescription Count",
        fill = "Specialty"
    ) +
    coord_cartesian(ylim = c(0, 10000)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1)
    )

# Table: What is the distribution of prescription counts per doctor by year by specialty? (all specialties)
prescription_counts_by_spec_table <- prescription_counts_with_spec %>%
    group_by(YEAR, SPECIALTY) %>%
    summarize(
        avg_count = mean(COUNT, na.rm = TRUE),
        median_count = median(COUNT, na.rm = TRUE),
        min_count = min(COUNT, na.rm = TRUE),
        max_count = max(COUNT, na.rm = TRUE),
        n_doctors = n(),
        .groups = "drop"
    ) %>%
    arrange(YEAR, SPECIALTY)

# Print the table using gtsummary
prescription_counts_by_spec_table %>%
    gt() %>%
    tab_header(
        title = "Distribution of Prescription Counts per Doctor by Year and Specialty"
    ) %>%
    cols_label(
        avg_count = "Average Count",
        median_count = "Median Count",
        min_count = "Minimum Count",
        max_count = "Maximum Count",
        n_doctors = "Number of Doctors"
    )


# Convert the table to a data frame for printing
prescription_counts_by_spec_table_df <- as.data.frame(prescription_counts_by_spec_table)

# Split the table into chunks of 40 rows per page
table_chunks <- split(prescription_counts_by_spec_table_df, 
                      ceiling(seq_along(1:nrow(prescription_counts_by_spec_table_df))/40))

# Save the table as a PDF file (split into pages if too long)
pdf(
    file = file.path(plot_dir, paste0("prescription_counts_by_spec_table_", timestamp, ".pdf")),
    width = 14,
    height = 8
)
for (i in seq_along(table_chunks)) {
    grid.newpage()
    grid.draw(gridExtra::tableGrob(table_chunks[[i]], rows = NULL))
}
dev.off()


# Plot: What percentage of prescriptions for their patients have not been redeemed?
# Load the redeemed / non-redeemed prescriptions stats
presc_redeemed_stats <- fread("/path/to/presc_redeemed_stats.csv") # columns: DOCTOR_ID, YEAR, COUNT_PRESCRIPTIONS, COUNT_IMPUTED

# Preprocess the data
presc_redeemed_stats <- presc_redeemed_stats %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(YEAR = as.numeric(YEAR))

# Aggregate by year and calculate the percentage of non-redeemed prescriptions
presc_redeemed_stats <- presc_redeemed_stats %>%
    group_by(YEAR) %>%
    summarize(
        COUNT_PRESCRIPTIONS = sum(COUNT_PRESCRIPTIONS, na.rm = TRUE),
        COUNT_IMPUTED = sum(COUNT_IMPUTED, na.rm = TRUE),
        PERCENT_NON_REDEEMED = (sum(COUNT_PRESCRIPTIONS - COUNT_IMPUTED, na.rm = TRUE) / sum(COUNT_PRESCRIPTIONS, na.rm = TRUE)) * 100
    )

# Plot: Percentage of non-redeemed prescriptions over the years
# ggplot(presc_redeemed_stats, aes(x = YEAR, y = PERCENT_NON_REDEEMED)) +
#    geom_line() +
#    geom_point() +
#    labs(
#        title = "Percentage of Non-Redeemed Prescriptions Over the Years",
#        x = "Year",
#        y = "Percentage of Non-Redeemed Prescriptions"
#    ) +
#    theme_minimal()

# Plot: Redeemed vs Non-Redeemed Prescriptions by Year as line plots in same diagram
presc_redeemed_stats_long <- presc_redeemed_stats %>%
    pivot_longer(cols = c(COUNT_PRESCRIPTIONS, COUNT_IMPUTED), 
                 names_to = "Prescription_Type", 
                 values_to = "Count") %>%
    mutate(Prescription_Type = recode(Prescription_Type, 
                                      COUNT_PRESCRIPTIONS = "Redeemed", 
                                      COUNT_IMPUTED = "Non-Redeemed"))

# Plot the line chart   
presc_redeemed_line_plot <- ggplot(presc_redeemed_stats_long, aes(x = YEAR, y = Count, color = Prescription_Type)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
        title = "Redeemed vs Non-Redeemed Prescriptions by Year",
        x = "Year",
        y = "Count of Prescriptions",
        color = "Prescription Type"
    ) +
    scale_color_manual(values = c("Redeemed" = "steelblue", "Non-Redeemed" = "orange")) +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1)
    )

# Save the line plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("presc_redeemed_line_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(presc_redeemed_line_plot)
dev.off()