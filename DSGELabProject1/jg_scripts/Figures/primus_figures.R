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
library(broom)

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
    right_join(
        tibble(DOCTOR_ID = doctor_IDs$V1),
        by = "DOCTOR_ID"
    ) %>%
    mutate(is_married = ifelse(is.na(is_married), FALSE, is_married),
           Marital_Status = ifelse(is_married, "Married", "Not Married")) %>%
    count(Marital_Status) %>%
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
    mutate(perc_married = 100 * n_marriages*2 / n_doctors) %>%     # *2 because each marriage involves 2 doctors
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


# Figure: Enrichment of Within-Specialty Marriages vs Between-Specialty Marriages

# For each specialty, calculate:
# - P(within) = probability a doctor marries someone of the same specialty
# - P(between) = probability a doctor marries someone of a different specialty
# - Enrichment = P(within) / mean(P(between)) for that specialty

# Prepare doctor-specialty lookup (non-empty specialties only)
doctor_specialty <- doctor_characteristics %>%
    filter(!is.na(INTERPRETATION) & INTERPRETATION != "") %>%
    select(DOCTOR_ID, SPECIALTY = INTERPRETATION)

# Find all marriages where both partners are doctors with a specialty
doctor_spouses <- doctors_family %>%
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    inner_join(doctor_specialty, by = c("DOCTOR_ID" = "DOCTOR_ID")) %>%
    inner_join(doctor_specialty, by = c("RELATIVE_ID" = "DOCTOR_ID"), suffix = c("_DOCTOR", "_SPOUSE"))

# For each specialty, count marriages to each other specialty
# Avoid double-counting only for within-specialty marriages (i.e., when SPECIALTY_DOCTOR == SPECIALTY_SPOUSE)
marriage_matrix <- doctor_spouses %>%
    mutate(
        # For within-specialty, keep only DOCTOR_ID < RELATIVE_ID; for between-specialty, keep all
        keep = if_else(SPECIALTY_DOCTOR == SPECIALTY_SPOUSE, DOCTOR_ID < RELATIVE_ID, TRUE)
    ) %>%
    filter(keep) %>%
    count(SPECIALTY_DOCTOR, SPECIALTY_SPOUSE) %>%
    group_by(SPECIALTY_DOCTOR) %>%
    mutate(
        total_marriages = sum(n),
        prob = n / total_marriages
    ) %>%
    ungroup()

# For each specialty, get:
# - P(within): probability of marrying within same specialty
# - mean P(between): mean probability of marrying any other specialty
enrichment_df <- marriage_matrix %>%
    group_by(SPECIALTY_DOCTOR) %>%
    summarize(
        prob_within = prob[SPECIALTY_DOCTOR == SPECIALTY_SPOUSE],
        prob_between_mean = mean(prob[SPECIALTY_DOCTOR != SPECIALTY_SPOUSE]),
        enrichment = prob_within / prob_between_mean,
        n_marriages = sum(n)
    ) %>%
    filter(n_marriages >= 10) %>% # privacy: only show specialties with >=10 marriages
    arrange(desc(enrichment))

# Plot: Enrichment of within-specialty marriage vs between-specialty marriage
enrichment_plot <- ggplot(enrichment_df, aes(x = enrichment, y = reorder(SPECIALTY_DOCTOR, enrichment))) +
    geom_bar(stat = "identity", fill = "#1b9e77") +
    geom_text(aes(label = round(enrichment, 2)), hjust = -0.1, size = 4) +
    labs(
        title = "Enrichment of Within-Specialty Marriages vs Between-Specialty Marriages",
        x = "Enrichment Ratio (Within / Mean Between)",
        y = "Specialty"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("within_specialty_enrichment_plot_", timestamp, ".pdf")),
    width = 12,
    height = 8
)
print(enrichment_plot)
dev.off()


# Save the Doctors' Family Dynamics in one figure
pdf(
    file = file.path(plot_dir, paste0("doctors_family_dynamics_", timestamp, ".pdf")),
    width = 14.5,
    height = 8
)
grid.arrange(married_status_plot, spec_marriage_count_plot, spec_marriage_perc_plot, ncol = 3)
dev.off()


# Plot: Zoom into Psychiatrists Marriages accross Specialties
# Filter for psychiatrists
doctor_spouses_psychiatrists <- doctor_spouses %>%
    filter(SPECIALTY_DOCTOR == "Psychiatry") %>%
    mutate(
        # For within-specialty, keep only DOCTOR_ID < RELATIVE_ID; for between-specialty, keep all
        keep = if_else(SPECIALTY_DOCTOR == SPECIALTY_SPOUSE, DOCTOR_ID < RELATIVE_ID, TRUE)
    ) %>%
    filter(keep) %>%
    group_by(SPECIALTY_SPOUSE) %>%
    summarize(
        n_marriages = n(), 
        .groups = "drop"
    ) %>%
    ungroup()

# Calculate percentage of each specialty among psychiatrist marriages
doctor_spouses_psychiatrists <- doctor_spouses_psychiatrists %>%
    mutate(
        perc = 100 * n_marriages / sum(n_marriages),
        label = paste0(round(perc, 1), "%")
    ) %>%
    # keep only top 10
    arrange(desc(perc)) %>%
    slice_head(n = 10)


# Plot: Bar plot of psychiatrist marriages by spouse specialty (percentage)
psychiatrist_marriages_plot <- ggplot(doctor_spouses_psychiatrists, aes(x = reorder(SPECIALTY_SPOUSE, perc), y = perc)) +
    geom_bar(stat = "identity", fill = "#7570b3") +
    geom_text(aes(label = label), hjust = -0.1, size = 4) +
    coord_flip() +
    labs(
        title = "Distribution of Psychiatrist Marriages Across Spouse Specialties",
        x = "Spouse Specialty",
        y = "Percentage of Psychiatrist Marriages (%)"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("psychiatrist_marriages_by_spouse_specialty_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(psychiatrist_marriages_plot)
dev.off()


# Save the Doctors' Family Dynamics V2 in one figure
pdf(
    file = file.path(plot_dir, paste0("doctors_family_dynamics_v2_", timestamp, ".pdf")),
    width = 28,
    height = 8
)
grid.arrange(married_status_plot, spec_marriage_count_plot, psychiatrist_marriages_plot, ncol = 3)
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

# 4b) Pair each non-doctor to the doctor with the same (SEX, age, rn)
matched_nondocs <- matched_nondocs %>%
    left_join(
        matched_docs %>% select(SEX, BIRTH_DATE, rn, matched_doctor_id = ID),
        by = c("SEX", "BIRTH_DATE", "rn")
    )

# 5) Stack them together and drop helper columns
matched_df <- bind_rows(
    matched_docs %>% mutate(matched_doctor_id = ID),
    matched_nondocs
) %>%
    select(-rn, -n_match)

# matched_df now has 1:1 matched doctors vs. non‐doctors, 
# exactly balanced on SEX & age, and for non-doctors, matched_doctor_id gives the paired doctor.
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


# Add specialty to matched_df based on matched_doctor_id
matched_df <- matched_df %>%
    left_join(
        doctor_characteristics %>% select(DOCTOR_ID, SPECIALTY = INTERPRETATION),
        by = c("matched_doctor_id" = "DOCTOR_ID")
    ) %>%
    mutate(SPECIALTY = ifelse(is.na(SPECIALTY) | SPECIALTY == "", "NO SPECIALTY", SPECIALTY))

# Find specialties with over 500 doctors
large_specialties <- doctor_characteristics %>%
    mutate(SPECIALTY = ifelse(is.na(INTERPRETATION) | INTERPRETATION == "", "NO SPECIALTY", INTERPRETATION)) %>%
    count(SPECIALTY) %>%
    filter(n > 500) %>%
    pull(SPECIALTY)

# Filter matched_df to only those specialties
matched_df_large_spec <- matched_df %>%
    filter(SPECIALTY %in% large_specialties)

# Fit Cox model for each specialty and extract HR, CI, p-value
cox_stats <- lapply(large_specialties, function(spec) {
    dat <- matched_df_large_spec %>% filter(SPECIALTY == spec)
    if (nrow(dat) > 0 && length(unique(dat$is_doctor)) == 2) {
        model <- coxph(Surv(age, DEATH) ~ is_doctor, data = dat)
        s <- summary(model)
        hr <- s$coef[1, "exp(coef)"]
        lower <- s$conf.int[1, "lower .95"]
        upper <- s$conf.int[1, "upper .95"]
        pval <- s$coef[1, "Pr(>|z|)"]
        data.frame(
            SPECIALTY = spec,
            HR = hr,
            HR_CI = sprintf("%.2f (%.2f–%.2f)", hr, lower, upper),
            pval = pval,
            pval_txt = ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval))
        )
    } else {
        data.frame(
            SPECIALTY = spec,
            HR = NA,
            HR_CI = NA,
            pval = NA,
            pval_txt = NA
        )
    }
})
cox_stats_df <- do.call(rbind, cox_stats)

# Fit Kaplan-Meier for each specialty and doctor status
km_fit_spec <- survfit(Surv(age, DEATH) ~ is_doctor + SPECIALTY, data = matched_df_large_spec)

# Convert survfit object to data frame for ggplot2 (with CI and facet by specialty)
km_spec_df <- 
    broom::tidy(km_fit_spec) %>%
    mutate(
        Group = ifelse(str_detect(strata, "is_doctor=1"), "Doctor", "Not Doctor"),
        SPECIALTY = str_replace(strata, "is_doctor=\\d+\\,SPECIALTY=", ""),
        SPECIALTY = str_replace(SPECIALTY, "is_doctor=\\d+", ""),
        SPECIALTY = str_replace(SPECIALTY, "SPECIALTY=", ""),
        SPECIALTY = str_remove(SPECIALTY, "^,")
    )

# Ensure SPECIALTY columns are character and trimmed for proper join
km_spec_df <- km_spec_df %>%
    mutate(SPECIALTY = as.character(trimws(SPECIALTY)))
cox_stats_df <- cox_stats_df %>%
    mutate(SPECIALTY = as.character(trimws(SPECIALTY)))

# Merge HR/CI/pval for annotation
km_spec_df <- km_spec_df %>%
    left_join(cox_stats_df, by = "SPECIALTY")

# Create annotation data for each facet (one row per specialty)
facet_annots <- cox_stats_df %>%
    mutate(
        label = ifelse(is.na(HR_CI), "N/A", paste0("HR: ", HR_CI, "\np = ", pval_txt))
    )

# Calculate total N per specialty (doctors + non-doctors)
facet_annots <- cox_stats_df %>%
    left_join(
        matched_df_large_spec %>%
            group_by(SPECIALTY) %>%
            summarize(N = n(), .groups = "drop"),
        by = "SPECIALTY"
    ) %>%
    mutate(
        label = ifelse(
            is.na(HR_CI),
            paste0("N = ", N, "\nN/A"),
            paste0("N(doctors) = ", N, "\nHR: ", HR_CI, "\np = ", pval_txt)
        )
    )

# Plot with ggplot2, faceted by specialty, with HR/CI/p-value annotation and total N
km_spec_plot <- ggplot(km_spec_df, aes(x = time, y = estimate, color = Group, fill = Group)) +
    geom_step(size = 1.1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.18, color = NA) +
    labs(
        title = "Kaplan-Meier Survival Curves: Doctors vs Not Doctors by Specialty",
        x = "Age",
        y = "Survival Probability",
        color = "Group",
        fill = "Group"
    ) +
    scale_color_manual(values = c("Not Doctor" = "gray70", "Doctor" = "steelblue")) +
    scale_fill_manual(values = c("Not Doctor" = "gray70", "Doctor" = "steelblue")) +
    facet_wrap(~ SPECIALTY, ncol = 2, scales = "free_y") +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 12)
    ) +
    geom_text(
        data = facet_annots,
        aes(x = Inf, y = Inf, label = label),
        hjust = 1.05, vjust = 1.1, size = 4, color = "black",
        inherit.aes = FALSE
    )

# Save the faceted plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("km_plot_doctor_vs_nondoctor_by_specialty_", timestamp, ".pdf")),
    width = 14,
    height = 10
)
print(km_spec_plot)
dev.off()

# Combine Cox model results for all specialties into a single summary table
cox_stats_combined <- lapply(large_specialties, function(spec) {
    dat <- matched_df_large_spec %>% filter(SPECIALTY == spec)
    if (nrow(dat) > 0 && length(unique(dat$is_doctor)) == 2) {
        model <- coxph(Surv(age, DEATH) ~ is_doctor, data = dat)
        s <- summary(model)
        tibble(
            Specialty = spec,
            HR = s$coef[1, "exp(coef)"],
            CI_lower = s$conf.int[1, "lower .95"],
            CI_upper = s$conf.int[1, "upper .95"],
            p_value = s$coef[1, "Pr(>|z|)"]
        )
    } else {
        tibble(
            Specialty = spec,
            HR = NA_real_,
            CI_lower = NA_real_,
            CI_upper = NA_real_,
            p_value = NA_real_
        )
    }
}) %>% bind_rows()

# Format the table for display
cox_stats_combined <- cox_stats_combined %>%
    mutate(
        HR_CI = sprintf("%.2f (%.2f–%.2f)", HR, CI_lower, CI_upper),
        p_value_fmt = ifelse(is.na(p_value), NA, ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)))
    ) %>%
    select(Specialty, HR_CI, p_value_fmt)

# Rename columns for clarity
colnames(cox_stats_combined) <- c("Specialty", "Hazard Ratio (95% CI)", "p-value")

# Print as a gt summary table
cox_gt_tbl <- cox_stats_combined %>%
    gt() %>%
    tab_header(
        title = "Cox Model: Doctor vs Non-Doctor Survival by Specialty"
    )

# # Save the Cox model summary table as a nicely formatted PNG (avoids Chrome/webshot PDF issues)
# cox_gt_tbl %>%
#     gtsave(
#         filename = file.path(plot_dir, paste0("cox_model_by_specialty_", timestamp, ".png")),
#         expand = 10
#     )

# Plot: KM curves of all specialties in one plot, each in a different color

# Prepare data: get KM curves for each specialty (doctors only, large specialties)
km_spec_all <- survfit(Surv(age, DEATH) ~ SPECIALTY, data = matched_df %>% filter(SPECIALTY %in% large_specialties, is_doctor == 1))

# Convert to tidy data frame for ggplot2
km_spec_all_df <- broom::tidy(km_spec_all) %>%
    mutate(SPECIALTY = str_replace(strata, "SPECIALTY=", ""))

# Assign a color palette
n_spec <- length(unique(km_spec_all_df$SPECIALTY))
spec_colors <- scales::hue_pal()(n_spec)
names(spec_colors) <- unique(km_spec_all_df$SPECIALTY)

# Plot all specialties' KM curves in one plot
km_all_spec_plot <- ggplot(km_spec_all_df, aes(x = time, y = estimate, color = SPECIALTY)) +
    geom_step(size = 1.1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = SPECIALTY), alpha = 0.13, color = NA) +
    scale_color_manual(values = spec_colors) +
    scale_fill_manual(values = spec_colors) +
    labs(
        title = "Kaplan-Meier Survival Curves by Specialty (Doctors Only)",
        x = "Age",
        y = "Survival Probability",
        color = "Specialty",
        fill = "Specialty"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "right"
    )

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("km_plot_all_specialties_doctors_", timestamp, ".pdf")),
    width = 12,
    height = 8
)
print(km_all_spec_plot)
dev.off()

# Prepare data: doctors only, large specialties
km_spec_all_fit <- survfit(Surv(age, DEATH) ~ SPECIALTY, data = matched_df %>% filter(SPECIALTY %in% large_specialties & SPECIALTY != "NO SPECIALTY", is_doctor == 1))

# Plot with risk table (percentage alive)
km_spec_all_survminer <- ggsurvplot(
    km_spec_all_fit,
    data = matched_df %>% filter(SPECIALTY %in% large_specialties  & SPECIALTY != "NO SPECIALTY", is_doctor == 1),
    censor = FALSE,
    risk.table = "percentage",
    risk.table.title = "Percentage Alive",
    risk.table.height = 0.25,
    risk.table.y.text.col = TRUE,
    risk.table.y.text = FALSE,
    palette = spec_colors,
    legend.title = "Specialty",
    legend.labs = large_specialties[large_specialties != "NO SPECIALTY"],
    xlab = "Age",
    ylab = "Survival Probability",
    title = "Kaplan-Meier Survival Curves by Specialty (Doctors Only)",
    ggtheme = theme_minimal(base_size = 15),
    risk.table.col = "strata",
    risk.table.type = "percentage"
)

# Save the plot with risk table as a pdf file
pdf(
    file = file.path(plot_dir, paste0("km_plot_all_specialties_doctors_with_risktable_", timestamp, ".pdf")),
    width = 12,
    height = 10
)
print(km_spec_all_survminer)
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

doctor_characteristics <- doctor_characteristics %>% 
    mutate(START_YEAR = year(as.Date(START_DATE)))

# Calculate the stats of prescriptions per practicing year for doctors
prescription_counts_years <- prescription_counts %>%
    left_join(doctor_characteristics %>% select(DOCTOR_ID, START_YEAR), by = "DOCTOR_ID") %>%
    mutate(practicing_year = YEAR - START_YEAR + 1) %>%
    filter(practicing_year > 0) %>% # QC
    group_by(practicing_year) %>%
    # Aggregate at the doctor level first to avoid duplicate DOCTOR_IDs per year
    group_modify(~ {
        doctor_counts <- .x %>%
            group_by(DOCTOR_ID) %>%
            summarize(COUNT = sum(COUNT, na.rm = TRUE), .groups = "drop")
        n_doctors <- nrow(doctor_counts)
        avg_count <- mean(doctor_counts$COUNT, na.rm = TRUE)
        sd_count <- ifelse(n_doctors > 1, sd(doctor_counts$COUNT, na.rm = TRUE), NA_real_)
        se <- ifelse(n_doctors > 1, sd_count / sqrt(n_doctors), NA_real_)
        ci_lower <- ifelse(n_doctors > 1, avg_count - qt(0.975, df = n_doctors - 1) * se, NA_real_)
        ci_upper <- ifelse(n_doctors > 1, avg_count + qt(0.975, df = n_doctors - 1) * se, NA_real_)
        median <- median(doctor_counts$COUNT, na.rm = TRUE)
        perc_25 <- quantile(doctor_counts$COUNT, 0.25, na.rm = TRUE)
        perc_75 <- quantile(doctor_counts$COUNT, 0.75, na.rm = TRUE)
        tibble(
            COUNT = sum(doctor_counts$COUNT, na.rm = TRUE),
            n_doctors = n_doctors,
            avg_count = avg_count,
            sd_count = sd_count,
            se = se,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            median = median,
            perc_25 = perc_25,
            perc_75 = perc_75
        )
    }) %>%
    ungroup()

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


# Plot: Median and IQR of prescription counts per practicing year (years 1 to 30)
prescription_counts_years_plot <- prescription_counts_years %>%
    filter(practicing_year >= 1, practicing_year <= 30) %>%
    ggplot(aes(x = practicing_year)) +
    geom_line(aes(y = median), color = "steelblue", size = 1.2) +
    geom_ribbon(aes(ymin = perc_25, ymax = perc_75), fill = "steelblue", alpha = 0.2) +
    labs(
        title = "Median and IQR of Prescription Counts per Practicing Year",
        x = "Practicing Year",
        y = "Prescription Count"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
    )

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("prescription_counts_years_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(prescription_counts_years_plot)
dev.off()

# Plot: Mean and 95% CI of prescription counts per practicing year (years 1 to 30)
prescription_counts_years_mean_plot <- prescription_counts_years %>%
    filter(practicing_year >= 1, practicing_year <= 30) %>%
    ggplot(aes(x = practicing_year)) +
    geom_line(aes(y = avg_count), color = "darkorange", size = 1.2) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "darkorange", alpha = 0.2) +
    labs(
        title = "Mean and 95% CI of Prescription Counts per Practicing Year",
        x = "Practicing Year",
        y = "Prescription Count"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
    )

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("prescription_counts_years_mean_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(prescription_counts_years_mean_plot)
dev.off()


# Plot: Median/IQR and Mean/95% CI of prescription counts per practicing year (years 1 to 30) in one plot
prescription_counts_years_combined_plot <- prescription_counts_years %>%
    filter(practicing_year >= 1, practicing_year <= 30) %>%
    ggplot(aes(x = practicing_year)) +
    # Plot 95% CI (orange) first so it is underneath
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CI"), alpha = 0.35) +
    # Plot IQR (blue) with partial transparency so orange is visible underneath
    geom_ribbon(aes(ymin = perc_25, ymax = perc_75, fill = "IQR"), alpha = 0.18) +
    # Median and Mean lines
    geom_line(aes(y = median, color = "Median"), size = 1.2) +
    geom_line(aes(y = avg_count, color = "Mean"), size = 1.2) +
    scale_color_manual(
        name = "Line",
        values = c("Median" = "steelblue", "Mean" = "darkorange")
    ) +
    scale_fill_manual(
        name = "Interval",
        values = c("IQR" = "steelblue", "95% CI" = "darkorange"),
        labels = c("IQR" = "Interquartile Range", "95% CI" = "95% Confidence Interval")
    ) +
    labs(
        title = "Prescription Counts per Practicing Year: Median/IQR and Mean/95% CI",
        x = "Practicing Year",
        y = "Prescription Count"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.position = "bottom"
    )

# Save the combined plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("prescription_counts_years_combined_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(prescription_counts_years_combined_plot)
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

# Plot: Percentage of non-redeemed prescriptions by year
presc_non_redeemed_plot <- presc_redeemed_stats %>%
    filter(YEAR >= 2017, YEAR <= 2021) %>%
    ggplot(aes(x = YEAR, y = PERCENT_NON_REDEEMED)) +
    geom_line(color = "orange", size = 1.2) +
    geom_point(color = "orange", size = 2) +
    labs(
        title = "Percentage of Non-Redeemed Prescriptions (2017–2021)",
        x = "Year",
        y = "Percentage of Non-Redeemed Prescriptions"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1)
    )

# Save plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("presc_non_redeemed_plot_2017_2021_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(presc_non_redeemed_plot)
dev.off()



#===============================================================================
# Figures: Doctor Patient Interaction
#===============================================================================
# Load the data
doctor_IDs <- fread("/path/to/doctor_IDs.csv", header = F)
doctor_characteristics <- fread("/path/to/doctor_characteristics.csv")
dvv <- fread("/path/to/dvv.csv")
dp_summary <- fread("/path/to/doctor_patient_summary.csv") # columns: DOCTOR_ID, YEAR, TotalPatients, UniquePatients, TotalVisits, Prescriptions, DiagnosisAvohilmo, DiagnosisHilmo, SelfPrescriptions, SelfDiagnosis
pd_summary <- fread("/path/to/patient_doctor_summary.csv") # columns: PATIENT_ID, YEAR, TotalDoctors, UniqueDoctors, TotalVisits, Prescriptions, DiagnosisAvohilmo, DiagnosisHilmo

# Preprocess the data
doctor_characteristics <- doctor_characteristics %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(
        BIRTH_YEAR = as.numeric(year(as.Date(BIRTH_DATE))),
        BIRTH_YEAR = ifelse(BIRTH_YEAR < 1900 | BIRTH_YEAR > 2023, NA, BIRTH_YEAR),
        START_YEAR = as.numeric(year(as.Date(START_DATE))),
        END_YEAR = as.numeric(year(as.Date(END_DATE))))

dvv <- dvv %>% rename(ID = FID,
                     BIRTH_DATE = `Syntymä-päivä`,
                     DEATH_DATE = Kuolinpäivä,
                     SEX = `Suku-.puoli`) %>% 
                select(ID, SEX, BIRTH_DATE, DEATH_DATE) %>% 
                mutate(BIRTH_DATE = as.Date(as.character(BIRTH_DATE), format = "%Y%m%d"),
                       DEATH_DATE = as.Date(as.character(DEATH_DATE), format = "%Y%m%d"))

# DP Summary: Preprocess the data
dp_summary <- dp_summary %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(YEAR = as.numeric(YEAR)) %>% 
    left_join(doctor_characteristics %>% select(DOCTOR_ID, START_YEAR, END_YEAR, SEX, SPECIALTY = INTERPRETATION), by = "DOCTOR_ID") %>%
    mutate(practicing_year = YEAR - START_YEAR + 1,
           randomization_fct = UniquePatients / TotalVisits,
           virtual_start_year = ifelse(START_YEAR < 1998, 1998, START_YEAR),
           virtual_practicing_years = END_YEAR - virtual_start_year + 1) %>%
    filter(practicing_year > 0) # QC

# Calculate for each doctor the total number of TotalVisits, TotalPatients and UniquePatients, keeping the SPECIALTY and virtual_practicing_years
dp_summary_stats_total <- dp_summary %>%
    group_by(DOCTOR_ID) %>%
    summarize(
        TotalVisits = sum(TotalVisits, na.rm = TRUE),
        TotalPatients = sum(TotalPatients, na.rm = TRUE),
        UniquePatients = sum(UniquePatients, na.rm = TRUE),
        Total_Prescriptions = sum(Prescriptions, na.rm = TRUE),
        SPECIALTY = first(SPECIALTY),
        virtual_practicing_years = first(virtual_practicing_years),
        SEX = first(SEX)
    ) %>%
    mutate(
        SEX = factor(SEX, levels = c(1,2), labels = c("Male", "Female")),
        VisitsPerYear = TotalVisits / virtual_practicing_years,
        PatientsPerYear = TotalPatients / virtual_practicing_years,
        UniquePatientsPerYear = UniquePatients / virtual_practicing_years,
        PrescriptionsPerYear = Total_Prescriptions / virtual_practicing_years
    ) %>%
    ungroup()

# Summary by sex and virtual practicing year
dp_summary_stats_total_summary <- dp_summary_stats_total %>%
    group_by(SEX, virtual_practicing_years) %>%
    summarize(
        n = n(),
        mean_TotalVisits = mean(TotalVisits, na.rm = TRUE),
        sd_TotalVisits = sd(TotalVisits, na.rm = TRUE),
        se_TotalVisits = sd_TotalVisits / sqrt(n),
        ci_lower_TotalVisits = mean_TotalVisits - qt(0.975, df = n - 1) * se_TotalVisits,
        ci_upper_TotalVisits = mean_TotalVisits + qt(0.975, df = n - 1) * se_TotalVisits,
        median_TotalVisits = median(TotalVisits, na.rm = TRUE),
        perc25_TotalVisits = quantile(TotalVisits, 0.25, na.rm = TRUE),
        perc75_TotalVisits = quantile(TotalVisits, 0.75, na.rm = TRUE),

        mean_TotalPatients = mean(TotalPatients, na.rm = TRUE),
        sd_TotalPatients = sd(TotalPatients, na.rm = TRUE),
        se_TotalPatients = sd_TotalPatients / sqrt(n),
        ci_lower_TotalPatients = mean_TotalPatients - qt(0.975, df = n - 1) * se_TotalPatients,
        ci_upper_TotalPatients = mean_TotalPatients + qt(0.975, df = n - 1) * se_TotalPatients,
        median_TotalPatients = median(TotalPatients, na.rm = TRUE),
        perc25_TotalPatients = quantile(TotalPatients, 0.25, na.rm = TRUE),
        perc75_TotalPatients = quantile(TotalPatients, 0.75, na.rm = TRUE),

        mean_UniquePatients = mean(UniquePatients, na.rm = TRUE),
        sd_UniquePatients = sd(UniquePatients, na.rm = TRUE),
        se_UniquePatients = sd_UniquePatients / sqrt(n),
        ci_lower_UniquePatients = mean_UniquePatients - qt(0.975, df = n - 1) * se_UniquePatients,
        ci_upper_UniquePatients = mean_UniquePatients + qt(0.975, df = n - 1) * se_UniquePatients,
        median_UniquePatients = median(UniquePatients, na.rm = TRUE),
        perc25_UniquePatients = quantile(UniquePatients, 0.25, na.rm = TRUE),
        perc75_UniquePatients = quantile(UniquePatients, 0.75, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    filter(n >= 5) # QC: only include groups with at least 5 doctors


# Calculate the stats of doctor-patient interactions per practicing year per specialty
dp_summary_stats_by_spec <- dp_summary %>%
    filter(!is.na(SPECIALTY) & SPECIALTY != "") %>%
    group_by(practicing_year, SPECIALTY) %>%
    summarize(
        n_doctors = n(),
        avg_total_patients = mean(TotalPatients, na.rm = TRUE),
        sd_total_patients = sd(TotalPatients, na.rm = TRUE),
        se_total_patients = sd_total_patients / sqrt(n_doctors),
        ci_lower_total_patients = avg_total_patients - qt(0.975, df = n_doctors - 1) * se_total_patients,
        ci_upper_total_patients = avg_total_patients + qt(0.975, df = n_doctors - 1) * se_total_patients,
        median_total_patients = median(TotalPatients, na.rm = TRUE),
        perc25_total_patients = quantile(TotalPatients, 0.25, na.rm = TRUE),
        perc75_total_patients = quantile(TotalPatients, 0.75, na.rm = TRUE),

        avg_unique_patients = mean(UniquePatients, na.rm = TRUE),
        sd_unique_patients = sd(UniquePatients, na.rm = TRUE),
        se_unique_patients = sd_unique_patients / sqrt(n_doctors),
        ci_lower_unique_patients = avg_unique_patients - qt(0.975, df = n_doctors - 1) * se_unique_patients,
        ci_upper_unique_patients = avg_unique_patients + qt(0.975, df = n_doctors - 1) * se_unique_patients,
        median_unique_patients = median(UniquePatients, na.rm = TRUE),
        perc25_unique_patients = quantile(UniquePatients, 0.25, na.rm = TRUE),
        perc75_unique_patients = quantile(UniquePatients, 0.75, na.rm = TRUE),

        avg_total_visits = mean(TotalVisits, na.rm = TRUE),
        sd_total_visits = sd(TotalVisits, na.rm = TRUE),
        se_total_visits = sd_total_visits / sqrt(n_doctors),
        ci_lower_total_visits = avg_total_visits - qt(0.975, df = n_doctors - 1) * se_total_visits,
        ci_upper_total_visits = avg_total_visits + qt(0.975, df = n_doctors - 1) * se_total_visits,
        median_total_visits = median(TotalVisits, na.rm = TRUE),
        perc25_total_visits = quantile(TotalVisits, 0.25, na.rm = TRUE),
        perc75_total_visits = quantile(TotalVisits, 0.75, na.rm = TRUE),

        avg_randomization_fct = mean(randomization_fct, na.rm = TRUE),
        sd_randomization_fct = sd(randomization_fct, na.rm = TRUE),
        se_randomization_fct = sd_randomization_fct / sqrt(n_doctors),
        ci_lower_randomization_fct = avg_randomization_fct - qt(0.975, df = n_doctors - 1) * se_randomization_fct,
        ci_upper_randomization_fct = avg_randomization_fct + qt(0.975, df = n_doctors - 1) * se_randomization_fct,
        median_randomization_fct = median(randomization_fct, na.rm = TRUE),
        perc25_randomization_fct = quantile(randomization_fct, 0.25, na.rm = TRUE),
        perc75_randomization_fct = quantile(randomization_fct, 0.75, na.rm = TRUE)
    ) %>%
    ungroup()

# Self-prescriptions
dp_summary_selfpresc <- dp_summary %>% 
    group_by(DOCTOR_ID) %>%
    summarize(
        ever_selfprescribed = as.integer(any(SelfPrescription > 0, na.rm = TRUE)),
        SPECIALTY = first(SPECIALTY)
    ) %>%
    ungroup() %>% 
    group_by(SPECIALTY) %>%
    summarize(
        n_doctors = n(),
        n_selfprescribing = sum(ever_selfprescribed, na.rm = TRUE),
        perc_selfprescribing = (n_selfprescribing / n_doctors) * 100
    ) %>%
    ungroup() %>% 
    arrange(desc(perc_selfprescribing))

# PD Summary: Preprocess the data
pd_summary <- pd_summary %>%
    mutate(YEAR = as.numeric(YEAR)) %>% 
    left_join(dvv %>% select(ID, BIRTH_DATE, SEX), by = c("PATIENT_ID" = "ID")) %>% 
    mutate(BIRTH_YEAR = as.numeric(year(BIRTH_DATE)),
           age = YEAR - BIRTH_YEAR) %>%
    filter(age >= 0) # QC

# Calculate the stats of patient-doctor interactions per age year
pd_summary_stats_by_age <- pd_summary %>%
    group_by(age) %>%
    summarize(
        n_patients = n(),
        avg_total_doctors = mean(TotalDoctors, na.rm = TRUE),
        sd_total_doctors = sd(TotalDoctors, na.rm = TRUE),
        se_total_doctors = sd_total_doctors / sqrt(n_patients),
        ci_lower_total_doctors = avg_total_doctors - qt(0.975, df = n_patients - 1) * se_total_doctors,
        ci_upper_total_doctors = avg_total_doctors + qt(0.975, df = n_patients - 1) * se_total_doctors,
        median_total_doctors = median(TotalDoctors, na.rm = TRUE),
        perc25_total_doctors = quantile(TotalDoctors, 0.25, na.rm = TRUE),
        perc75_total_doctors = quantile(TotalDoctors, 0.75, na.rm = TRUE),

        avg_unique_doctors = mean(UniqueDoctors, na.rm = TRUE),
        sd_unique_doctors = sd(UniqueDoctors, na.rm = TRUE),
        se_unique_doctors = sd_unique_doctors / sqrt(n_patients),
        ci_lower_unique_doctors = avg_unique_doctors - qt(0.975, df = n_patients - 1) * se_unique_doctors,
        ci_upper_unique_doctors = avg_unique_doctors + qt(0.975, df = n_patients - 1) * se_unique_doctors,
        median_unique_doctors = median(UniqueDoctors, na.rm = TRUE),
        perc25_unique_doctors = quantile(UniqueDoctors, 0.25, na.rm = TRUE),
        perc75_unique_doctors = quantile(UniqueDoctors, 0.75, na.rm = TRUE),

        avg_total_visits_pd = mean(TotalVisits, na.rm = TRUE),
        sd_total_visits_pd = sd(TotalVisits, na.rm = TRUE),
        se_total_visits_pd = sd_total_visits_pd / sqrt(n_patients),
        ci_lower_total_visits_pd = avg_total_visits_pd - qt(0.975, df = n_patients - 1) * se_total_visits_pd,
        ci_upper_total_visits_pd = avg_total_visits_pd + qt(0.975, df = n_patients - 1) * se_total_visits_pd,
        median_total_visits_pd = median(TotalVisits, na.rm = TRUE),
        perc25_total_visits_pd = quantile(TotalVisits, 0.25, na.rm = TRUE),
        perc75_total_visits_pd = quantile(TotalVisits, 0.75, na.rm = TRUE),

        # Randomization factor: UniqueDoctors / TotalVisits
        avg_randomization_fct = mean(UniqueDoctors / TotalVisits, na.rm = TRUE),
        sd_randomization_fct = sd(UniqueDoctors / TotalVisits, na.rm = TRUE),
        se_randomization_fct = sd_randomization_fct / sqrt(n_patients),
        ci_lower_randomization_fct = avg_randomization_fct - qt(0.975, df = n_patients - 1) * se_randomization_fct,
        ci_upper_randomization_fct = avg_randomization_fct + qt(0.975, df = n_patients - 1) * se_randomization_fct,
        median_randomization_fct = median(UniqueDoctors / TotalVisits, na.rm = TRUE),
        perc25_randomization_fct = quantile(UniqueDoctors / TotalVisits, 0.25, na.rm = TRUE),
        perc75_randomization_fct = quantile(UniqueDoctors / TotalVisits, 0.75, na.rm = TRUE)
    ) %>%
    ungroup()

# Calculate the stats of patient-doctor interactions per age year per sex
pd_summary_stats_by_age_sex <- pd_summary %>%
    group_by(age, SEX) %>%
    summarize(
        n_patients = n(),
        avg_total_doctors = mean(TotalDoctors, na.rm = TRUE),
        sd_total_doctors = sd(TotalDoctors, na.rm = TRUE),
        se_total_doctors = sd_total_doctors / sqrt(n_patients),
        ci_lower_total_doctors = avg_total_doctors - qt(0.975, df = n_patients - 1) * se_total_doctors,
        ci_upper_total_doctors = avg_total_doctors + qt(0.975, df = n_patients - 1) * se_total_doctors,
        median_total_doctors = median(TotalDoctors, na.rm = TRUE),
        perc25_total_doctors = quantile(TotalDoctors, 0.25, na.rm = TRUE),
        perc75_total_doctors = quantile(TotalDoctors, 0.75, na.rm = TRUE),

        avg_unique_doctors = mean(UniqueDoctors, na.rm = TRUE),
        sd_unique_doctors = sd(UniqueDoctors, na.rm = TRUE),
        se_unique_doctors = sd_unique_doctors / sqrt(n_patients),
        ci_lower_unique_doctors = avg_unique_doctors - qt(0.975, df = n_patients - 1) * se_unique_doctors,
        ci_upper_unique_doctors = avg_unique_doctors + qt(0.975, df = n_patients - 1) * se_unique_doctors,
        median_unique_doctors = median(UniqueDoctors, na.rm = TRUE),
        perc25_unique_doctors = quantile(UniqueDoctors, 0.25, na.rm = TRUE),
        perc75_unique_doctors = quantile(UniqueDoctors, 0.75, na.rm = TRUE),

        avg_total_visits_pd = mean(TotalVisits, na.rm = TRUE),
        sd_total_visits_pd = sd(TotalVisits, na.rm = TRUE),
        se_total_visits_pd = sd_total_visits_pd / sqrt(n_patients),
        ci_lower_total_visits_pd = avg_total_visits_pd - qt(0.975, df = n_patients - 1) * se_total_visits_pd,
        ci_upper_total_visits_pd = avg_total_visits_pd + qt(0.975, df = n_patients - 1) * se_total_visits_pd,
        median_total_visits_pd = median(TotalVisits, na.rm = TRUE),
        perc25_total_visits_pd = quantile(TotalVisits, 0.25, na.rm = TRUE),
        perc75_total_visits_pd = quantile(TotalVisits, 0.75, na.rm = TRUE),

        # Randomization factor: UniqueDoctors / TotalVisits
        avg_randomization_fct = mean(UniqueDoctors / TotalVisits, na.rm = TRUE),
        sd_randomization_fct = sd(UniqueDoctors / TotalVisits, na.rm = TRUE),
        se_randomization_fct = sd_randomization_fct / sqrt(n_patients),
        ci_lower_randomization_fct = avg_randomization_fct - qt(0.975, df = n_patients - 1) * se_randomization_fct,
        ci_upper_randomization_fct = avg_randomization_fct + qt(0.975, df = n_patients - 1) * se_randomization_fct,
        median_randomization_fct = median(UniqueDoctors / TotalVisits, na.rm = TRUE),
        perc25_randomization_fct = quantile(UniqueDoctors / TotalVisits, 0.25, na.rm = TRUE),
        perc75_randomization_fct = quantile(UniqueDoctors / TotalVisits, 0.75, na.rm = TRUE)
    ) %>%
    ungroup()

# Plot: Median/IQR and Mean/95% CI of Doctor-Patient Interactions (randomization_fct) per Practicing Year (1 to 30) by Specialty (faceted)
# 1. Find top 8 specialties by doctor count
top_specialties <- doctor_characteristics %>%
    filter(!is.na(INTERPRETATION) & INTERPRETATION != "") %>%
    count(INTERPRETATION, sort = TRUE) %>%
    slice_head(n = 8) %>%
    pull(INTERPRETATION)

# 2. Add a new column to group others as "Other"
dp_summary_stats_by_spec <- dp_summary_stats_by_spec %>%
    mutate(SPECIALTY_GROUP = ifelse(SPECIALTY %in% top_specialties, SPECIALTY, "Other"))

# 3. Aggregate "Other" specialties (optional: sum n_doctors, take weighted mean, etc.)
dp_summary_stats_by_spec_grouped <- dp_summary %>%
    mutate(SPECIALTY_GROUP = ifelse(
        !is.na(SPECIALTY) & SPECIALTY %in% top_specialties, SPECIALTY, "Other"
    )) %>%
    group_by(practicing_year, SPECIALTY_GROUP) %>%
    summarize(
        n_doctors = n(),
        # randomization_fct stats
        avg_randomization_fct = mean(randomization_fct, na.rm = TRUE),
        sd_randomization_fct = sd(randomization_fct, na.rm = TRUE),
        se_randomization_fct = sd_randomization_fct / sqrt(n_doctors),
        ci_lower_randomization_fct = avg_randomization_fct - qt(0.975, df = n_doctors - 1) * se_randomization_fct,
        ci_upper_randomization_fct = avg_randomization_fct + qt(0.975, df = n_doctors - 1) * se_randomization_fct,
        median_randomization_fct = median(randomization_fct, na.rm = TRUE),
        perc25_randomization_fct = quantile(randomization_fct, 0.25, na.rm = TRUE),
        perc75_randomization_fct = quantile(randomization_fct, 0.75, na.rm = TRUE),
        # UniquePatients stats
        avg_unique_patients = mean(UniquePatients, na.rm = TRUE),
        sd_unique_patients = sd(UniquePatients, na.rm = TRUE),
        se_unique_patients = sd_unique_patients / sqrt(n_doctors),
        ci_lower_unique_patients = avg_unique_patients - qt(0.975, df = n_doctors - 1) * se_unique_patients,
        ci_upper_unique_patients = avg_unique_patients + qt(0.975, df = n_doctors - 1) * se_unique_patients,
        median_unique_patients = median(UniquePatients, na.rm = TRUE),
        perc25_unique_patients = quantile(UniquePatients, 0.25, na.rm = TRUE),
        perc75_unique_patients = quantile(UniquePatients, 0.75, na.rm = TRUE)
    ) %>%
    ungroup()

# 4. Plot using the grouped specialties
dp_summary_stats_by_spec_plot <- dp_summary_stats_by_spec_grouped %>%
    filter(practicing_year >= 1, practicing_year <= 30) %>%
    ggplot(aes(x = practicing_year)) +
    geom_ribbon(aes(ymin = ci_lower_randomization_fct, ymax = ci_upper_randomization_fct, fill = "95% CI"), alpha = 0.35) +
    geom_ribbon(aes(ymin = perc25_randomization_fct, ymax = perc75_randomization_fct, fill = "IQR"), alpha = 0.18) +
    geom_line(aes(y = median_randomization_fct, color = "Median"), size = 1.2) +
    geom_line(aes(y = avg_randomization_fct, color = "Mean"), size = 1.2) +
    scale_color_manual(name = "Line", values = c("Median" = "steelblue", "Mean" = "darkorange")) +
    scale_fill_manual(name = "Interval", values = c("IQR" = "steelblue", "95% CI" = "darkorange"),
                      labels = c("IQR" = "Interquartile Range", "95% CI" = "95% Confidence Interval")) +
    labs(
        title = "Doctor-Patient Randomization Factor per Practicing Year by Specialty (Top 8 + Other)",
        x = "Practicing Year",
        y = "Randomization Factor (UniquePatients / TotalVisits)"
    ) +
    facet_wrap(~ SPECIALTY_GROUP, ncol = 2, scales = "free_y") +
    coord_cartesian(ylim = c(0.3, 1.0)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        strip.text = element_text(size = 13)
    )

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("dp_interaction_randomization_fct_by_specialty_", timestamp, ".pdf")),
    width = 12,
    height = 10
)   
print(dp_summary_stats_by_spec_plot)
dev.off()


# Plot: How many patients does a doctor see on average over the course of their practice per practice year and by specialty?
dp_summary_stats_by_spec_unique_patients_plot <- dp_summary_stats_by_spec_grouped %>%
    filter(practicing_year >= 1, practicing_year <= 30) %>%
    ggplot(aes(x = practicing_year)) +
    geom_ribbon(aes(ymin = ci_lower_unique_patients, ymax = ci_upper_unique_patients, fill = "95% CI"), alpha = 0.35) +
    geom_ribbon(aes(ymin = perc25_unique_patients, ymax = perc75_unique_patients, fill = "IQR"), alpha = 0.18) +
    geom_line(aes(y = median_unique_patients, color = "Median"), size = 1.2) +
    geom_line(aes(y = avg_unique_patients, color = "Mean"), size = 1.2) +
    scale_color_manual(name = "Line", values = c("Median" = "steelblue", "Mean" = "darkorange")) +
    scale_fill_manual(name = "Interval", values = c("IQR" = "steelblue", "95% CI" = "darkorange"),
                      labels = c("IQR" = "Interquartile Range", "95% CI" = "95% Confidence Interval")) +
    labs(
        title = "Unique Patients per Doctor per Practicing Year by Specialty (Top 8 + Other)",
        x = "Practicing Year",
        y = "Unique Patients"
    ) +
    facet_wrap(~ SPECIALTY_GROUP, ncol = 2, scales = "free_y") +
    coord_cartesian(ylim = c(0, 1300)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        strip.text = element_text(size = 13)
    )

pdf(
    file = file.path(plot_dir, paste0("dp_interaction_unique_patients_by_specialty_", timestamp, ".pdf")),
    width = 12,
    height = 10
)
print(dp_summary_stats_by_spec_unique_patients_plot)
dev.off()

# Plot: Median/IQR and Mean/95% CI of Patient-Doctor Interactions (randomization_fct) per age (0 to 100)
pd_summary_stats_by_age_plot <- pd_summary_stats_by_age %>%
    filter(age >= 0, age <= 100) %>%
    ggplot(aes(x = age)) +
    geom_ribbon(aes(ymin = ci_lower_randomization_fct, ymax = ci_upper_randomization_fct, fill = "95% CI"), alpha = 0.35) +
    geom_ribbon(aes(ymin = perc25_randomization_fct, ymax = perc75_randomization_fct, fill = "IQR"), alpha = 0.18) +
    geom_line(aes(y = median_randomization_fct, color = "Median"), size = 1.2) +
    geom_line(aes(y = avg_randomization_fct, color = "Mean"), size = 1.2) +
    scale_color_manual(name = "Line", values = c("Median" = "steelblue", "Mean" = "darkorange")) +
    scale_fill_manual(name = "Interval", values = c("IQR" = "steelblue", "95% CI" = "darkorange"),
                      labels = c("IQR" = "Interquartile Range", "95% CI" = "95% Confidence Interval")) +
    labs(
        title = "Patient-Doctor Randomization Factor per Age",
        x = "Age",
        y = "Randomization Factor (UniqueDoctors / TotalVisits)"
    ) +
    coord_cartesian(ylim = c(0.3, 1.0)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom"
    )

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("pd_interaction_randomization_fct_by_age_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(pd_summary_stats_by_age_plot)
dev.off()

# Plot: How many doctors does a patient see on average over the course of their life per age?
pd_summary_stats_by_age_unique_doctors_plot <- pd_summary_stats_by_age %>%
    filter(age >= 0, age <= 100) %>%
    ggplot(aes(x = age)) +
    geom_ribbon(aes(ymin = ci_lower_total_doctors, ymax = ci_upper_total_doctors, fill = "95% CI"), alpha = 0.35) +
    geom_ribbon(aes(ymin = perc25_total_doctors, ymax = perc75_total_doctors, fill = "IQR"), alpha = 0.18) +
    geom_line(aes(y = median_total_doctors, color = "Median"), size = 1.2) +
    geom_line(aes(y = avg_total_doctors, color = "Mean"), size = 1.2) +
    scale_color_manual(name = "Line", values = c("Median" = "steelblue", "Mean" = "darkorange")) +
    scale_fill_manual(name = "Interval", values = c("IQR" = "steelblue", "95% CI" = "darkorange"),
                      labels = c("IQR" = "Interquartile Range", "95% CI" = "95% Confidence Interval")) +
    labs(
        title = "Total Doctors per Patient per Age",
        x = "Age",
        y = "Total Doctors"
    ) +
    coord_cartesian(ylim = c(0, 20)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom"
    )

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("pd_interaction_total_doctors_by_age_", timestamp, ".pdf")),
    width = 10,
    height = 8
)   
print(pd_summary_stats_by_age_unique_doctors_plot)
dev.off()


# Plot: Median/IQR and Mean/95% CI of Patient-Doctor Interactions (randomization_fct) per age (0 to 100) by SEX
pd_summary_stats_by_age_sex_plot <- pd_summary_stats_by_age_sex %>%
    filter(age >= 0, age <= 100) %>%
    mutate(SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))) %>%
    ggplot(aes(x = age)) +
    geom_ribbon(aes(ymin = ci_lower_randomization_fct, ymax = ci_upper_randomization_fct, fill = "95% CI"), alpha = 0.35) +
    geom_ribbon(aes(ymin = perc25_randomization_fct, ymax = perc75_randomization_fct, fill = "IQR"), alpha = 0.18) +
    geom_line(aes(y = median_randomization_fct, color = "Median"), size = 1.2) +
    geom_line(aes(y = avg_randomization_fct, color = "Mean"), size = 1.2) +
    scale_color_manual(
        name = "Line",
        values = c("Median" = "steelblue", "Mean" = "darkorange")
    ) +
    scale_fill_manual(
        name = "Interval",
        values = c("IQR" = "steelblue", "95% CI" = "darkorange"),
        labels = c("IQR" = "Interquartile Range", "95% CI" = "95% Confidence Interval")
    ) +
    labs(
        title = "Patient-Doctor Randomization Factor per Age by Sex",
        x = "Age",
        y = "Randomization Factor (UniqueDoctors / TotalVisits)"
    ) +
    facet_wrap(~ SEX, ncol = 1) +
    coord_cartesian(ylim = c(0.3, 1.0)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        strip.text = element_text(size = 13)
    )

pdf(
    file = file.path(plot_dir, paste0("pd_interaction_randomization_fct_by_age_sex_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(pd_summary_stats_by_age_sex_plot)
dev.off()


# Plot: How many doctors does a patient see on average over the course of their life per age by sex?
pd_summary_stats_by_age_sex_plot <- pd_summary_stats_by_age_sex %>%
    filter(age >= 0, age <= 100) %>%
    mutate(SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))) %>%
    ggplot(aes(x = age)) +
    geom_ribbon(aes(ymin = ci_lower_total_doctors, ymax = ci_upper_total_doctors, fill = "95% CI"), alpha = 0.35) +
    geom_ribbon(aes(ymin = perc25_total_doctors, ymax = perc75_total_doctors, fill = "IQR"), alpha = 0.18) +
    geom_line(aes(y = median_total_doctors, color = "Median"), size = 1.2) +
    geom_line(aes(y = avg_total_doctors, color = "Mean"), size = 1.2) +
    scale_color_manual(
        name = "Line",
        values = c("Median" = "steelblue", "Mean" = "darkorange")
    ) +
    scale_fill_manual(
        name = "Interval",
        values = c("IQR" = "steelblue", "95% CI" = "darkorange"),
        labels = c("IQR" = "Interquartile Range", "95% CI" = "95% Confidence Interval")
    ) +
    labs(
        title = "Total Doctors per Patient per Age by Sex",
        x = "Age",
        y = "Total Doctors"
    ) +
    facet_wrap(~ SEX, ncol = 1) +
    coord_cartesian(ylim = c(0, 20)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        strip.text = element_text(size = 13)
    )
# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("pd_interaction_total_doctors_by_age_sex_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(pd_summary_stats_by_age_sex_plot)
dev.off()   


# Plot: Self-prescriptions by specialty 
# Since all rates are high and close together, use a lollipop plot with a zoomed-in y-axis and highlight the lowest specialty

dp_selfpresc_plot <- dp_summary_selfpresc %>%
    filter(n_doctors > 0) %>%
    mutate(
        highlight = perc_selfprescribing == min(perc_selfprescribing, na.rm = TRUE)
    ) %>%
    ggplot(aes(x = reorder(SPECIALTY, perc_selfprescribing), y = perc_selfprescribing)) +
    geom_segment(aes(xend = SPECIALTY, y = 93, yend = perc_selfprescribing, color = highlight), size = 1.2) +
    geom_point(aes(color = highlight), size = 4) +
    # geom_text(aes(label = sprintf("%.1f%%", perc_selfprescribing)), 
    #           hjust = -0.1, vjust = 0.5, size = 4, color = "black") +
    scale_color_manual(values = c("TRUE" = "orange", "FALSE" = "steelblue"), guide = "none") +
    labs(
        title = "Self-Prescribing Rates by Specialty (Zoomed In)",
        x = "Specialty",
        y = "Self-Prescribing Rate (%)"
    ) +
    coord_flip(ylim = c(93, 100)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
    )

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("dp_selfpresc_by_specialty_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
print(dp_selfpresc_plot)
dev.off()


# Plot: Violin Plot of the density of Total Visits by Sex (on both side of the violin) using dp_summary_stats_total_summary
# Violin Plot of the density of Total Visits by Sex (on both sides of the violin)
dp_summary_stats_total_summary %>%
    ggplot(aes(x = SEX, y = mean_TotalVisits, fill = SEX)) +
    geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +
    geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
    labs(
        title = "Distribution of Mean Total Visits by Sex",
        x = "Sex",
        y = "Mean Total Visits"
    ) +
    scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
    )

# Plot: Violin Plot of the density of TotalVisitsPerYear by Sex (on both side of the violin) using dp_summary_stats_total
dp_visits_per_year_violin_plot <- dp_summary_stats_total %>%
    ggplot(aes(x = SEX, y = PrescriptionsPerYear, fill = SEX)) +
    geom_violin(trim = FALSE, scale = "width", alpha = 0.7) +
    geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
    labs(
        title = "Distribution of Total Prescriptions Per Year by Sex",
        x = "Sex",
        y = "Total Prescriptions Per Year"
    ) +
    scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
    )

pdf(
    file = file.path(plot_dir, paste0("dp_visits_per_year_violin_plot_", timestamp, ".pdf")),
    width = 8,
    height = 6
)
print(dp_visits_per_year_violin_plot)
dev.off()


#===============================================================================
# Figures: Doctors' Health Characteristics
#===============================================================================

# Load the data
doctor_IDs <- fread("/path/to/doctor_IDs.csv", header = F)
doctor_characteristics <- fread("/path/to/doctor_characteristics.csv")
prescriptions <- fread("/path/to/filtered/prescriptions.csv")
prescriptions_AH <- fread("/path/to/filtered/prescriptions_AH.csv") # Antihypertensives
prescriptions_VAC <- fread("/path/to/filtered/prescriptions_VAC.csv") # Vaccinations
diagnoses <- fread("/path/to/filtered/diagnoses.csv")
diagnoses_2 <- fread("/path/to/filtered/diagnoses_2.csv") # Additional diagnoses if needed
dvv <- fread("/path/to/dvv.csv")

# Preprocess the data
doctor_characteristics <- doctor_characteristics %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(
        BIRTH_YEAR = as.numeric(year(as.Date(BIRTH_DATE))),
        BIRTH_YEAR = ifelse(BIRTH_YEAR < 1900 | BIRTH_YEAR > 2023, NA, BIRTH_YEAR),
        START_YEAR = as.numeric(year(as.Date(START_DATE)))
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

# Add is_doctor column to prescriptions if PATIENT_ID is in doctor_IDs
prescriptions <- prescriptions %>%
    # select(PATIENT_ID, DOCTOR_ID, CODE, PRESCRIPTION_DATE) %>%
    select(PATIENT_ID, CODE) %>%
    mutate(is_doctor = PATIENT_ID %in% doctor_IDs$V1) %>% 
    distinct()

prescriptions_AH <- prescriptions_AH %>%
    select(PATIENT_ID, CODE) %>%
    mutate(is_doctor = PATIENT_ID %in% doctor_IDs$V1) %>% 
    distinct()

prescriptions_VAC <- prescriptions_VAC %>%
    select(PATIENT_ID, CODE) %>%
    mutate(is_doctor = PATIENT_ID %in% doctor_IDs$V1) %>% 
    distinct()

# Join rows of prescriptions with prescriptions_AH rbind()
prescriptions <- bind_rows(prescriptions, prescriptions_AH, prescriptions_VAC)

diagnoses <- diagnoses %>%
    select(PATIENT_ID, ICD10_CODE) %>%
    mutate(is_doctor = PATIENT_ID %in% doctor_IDs$V1) %>% 
    distinct()

diagnoses_2 <- diagnoses_2 %>%
    select(PATIENT_ID, ICD10_CODE) %>%
    mutate(is_doctor = PATIENT_ID %in% doctor_IDs$V1) %>% 
    distinct()

# Join rows of diagnoses with diagnoses_2 rbind()
diagnoses <- bind_rows(diagnoses, diagnoses_2) %>% 
    distinct()

# Create a matched cohort (sex, birthday) of doctors and non_doctors analogous to the survival analysis
# Match doctors with general population by age and sex
set.seed(123)

# 1) Compute how many doctors vs. non‐docs in each (SEX, age) cell,
#    then keep only strata where both groups exist and record n_match = min(#doc, #non).
counts <- dvv %>%
    group_by(SEX, BIRTH_DATE) %>%
    summarize(
        n_doc   = sum(is_doctor == 1),
        n_non   = sum(is_doctor == 0),
        n_match = min(n_doc, n_non),
        .groups = "drop"
    ) %>%
    filter(n_match > 0) %>%
    select(SEX, BIRTH_DATE, n_match)

# 2) Restrict to rows belonging to those “matchable” strata
dvv2 <- dvv %>%
    inner_join(counts, by = c("SEX", "BIRTH_DATE"))

# 3) From each (SEX, BIRTH_DATE), randomly pick exactly n_match doctors
matched_docs <- dvv2 %>%
    filter(is_doctor == 1) %>%
    group_by(SEX, BIRTH_DATE) %>%
    mutate(rn = sample(seq_len(n()), n())) %>%
    ungroup() %>%
    filter(rn <= n_match)

# 4) From each (SEX, BIRTH_DATE), randomly pick exactly n_match non‐doctors
matched_nondocs <- dvv2 %>%
    filter(is_doctor == 0) %>%
    group_by(SEX, BIRTH_DATE) %>%
    mutate(rn = sample(seq_len(n()), n())) %>%
    ungroup() %>%
    filter(rn <= n_match)

# 4b) Pair each non-doctor to the doctor with the same (SEX, BIRTH_DATE, rn)
matched_nondocs <- matched_nondocs %>%
    left_join(
        matched_docs %>% select(SEX, BIRTH_DATE, rn, matched_doctor_id = ID),
        by = c("SEX", "BIRTH_DATE", "rn")
    )

# 5) Stack them together and drop helper columns
matched_df <- bind_rows(
    matched_docs %>% mutate(matched_doctor_id = ID),
    matched_nondocs
) %>%
    select(-rn, -n_match)


# Prescriptions of cohort:
prescriptions_cohort <- prescriptions %>%
    filter(PATIENT_ID %in% matched_df$ID) 

# Diagnoses of cohort:
diagnoses_cohort <- diagnoses %>%
    filter(PATIENT_ID %in% matched_df$ID) 

N = nrow(doctor_IDs)

# Get ATC prevalences
# Helper: allow grouping of ATC prefixes under a common label
# Example: atc_groups = list("A10" = c("A10"), "C07" = c("C07"), "C09A" = c("C09AA", "C09AX"))
get_atc_prevalence <- function(atc_groups, prescriptions, n_doctors, n_nondoctors) {
    results <- lapply(names(atc_groups), function(group_label) {
        prefixes <- atc_groups[[group_label]]
        # Doctors
        n_doctors_on <- prescriptions %>%
            filter(is_doctor == TRUE, Reduce(`|`, lapply(prefixes, function(prefix) grepl(paste0("^", prefix), CODE)))) %>%
            distinct(PATIENT_ID) %>%
            nrow()
        # Non-doctors
        n_nondoctors_on <- prescriptions %>%
            filter(is_doctor == FALSE, Reduce(`|`, lapply(prefixes, function(prefix) grepl(paste0("^", prefix), CODE)))) %>%
            distinct(PATIENT_ID) %>%
            nrow()
        # Prevalence
        prevalence_doctors <- n_doctors_on / n_doctors
        prevalence_nondoctors <- n_nondoctors_on / n_nondoctors
        data.frame(
            ATC = group_label,
            n_doctors = n_doctors_on,
            prevalence_doctors = prevalence_doctors,
            n_nondoctors = n_nondoctors_on,
            prevalence_nondoctors = prevalence_nondoctors
        )
    })
    do.call(rbind, results)
}

# Define ATC prefixes
atc_prefixes <- list(
    "Antidiabetics" = c("A10"),
    "GLP1-RA" = c("A10BJ", "A10BX04", "A10BX10", "A10BX13", "A10BX14"),
    "Antihypertensives" = c("C07", "C08", "C09"),
    "Antidepressants" = c("N06A"),
    "Psycholeptics" = c("N05"),
    "Statins" = c("C10AA"),
    "PCSK9 Inhibitors" = c("C10AX13", "C10AX14", "C10AX16"),
    "COVID-19 Vaccines" = c("J07BN01", "J07BN02", "J07BN03", "J07BN04", "J07BN05") 
)

atc_prevalences_matched_cohort <- get_atc_prevalence(atc_prefixes, prescriptions_cohort, N, N) %>%
    mutate(
        # ATC = factor(ATC, levels = atc_prefixes),
        prevalence_doctors = round(prevalence_doctors * 100, 2),
        prevalence_nondoctors = round(prevalence_nondoctors * 100, 2),
        factor_doctor_vs_nondoctor = ifelse(prevalence_nondoctors == 0, NA, round(prevalence_doctors / prevalence_nondoctors, 2))
    ) 

atc_prevalences_matched_cohort %>%
    gt() %>%
    tab_header(
        title = "ATC Prevalence: Doctors vs Non-Doctors (Matched Cohort)"
    ) %>%
    cols_label(
        ATC = "ATC Code",
        n_doctors = "Doctors (n)",
        prevalence_doctors = "Doctors (%)",
        n_nondoctors = "Non-Doctors (n)",
        prevalence_nondoctors = "Non-Doctors (%)",
        factor_doctor_vs_nondoctor = "Doctor/Non-Doctor Ratio"
    ) %>%
    fmt_number(
        columns = c(prevalence_doctors, prevalence_nondoctors, factor_doctor_vs_nondoctor),
        decimals = 2
    )


# For diagnoses, we can use a similar approach to get the prevalence of specific diagnoses

# Get ICD prevalences
# Helper: allow grouping of ICD codes under a common label
# Example: icd_groups = list("J10" = c("J10"), "J07" = c("J07"), "F30" = c("F30.1", "F30.0"))
get_icd_prevalence <- function(icd_groups, diagnoses, n_doctors, n_nondoctors) {
    results <- lapply(names(icd_groups), function(group_label) {
        prefixes <- icd_groups[[group_label]]
        # Doctors
        n_doctors_on <- diagnoses %>%
            filter(is_doctor == TRUE, Reduce(`|`, lapply(prefixes, function(prefix) grepl(paste0("^", prefix), ICD10_CODE)))) %>%
            distinct(PATIENT_ID) %>%
            nrow()
        # Non-doctors
        n_nondoctors_on <- diagnoses %>%
            filter(is_doctor == FALSE, Reduce(`|`, lapply(prefixes, function(prefix) grepl(paste0("^", prefix), ICD10_CODE)))) %>%
            distinct(PATIENT_ID) %>%
            nrow()
        # Prevalence
        prevalence_doctors <- n_doctors_on / n_doctors
        prevalence_nondoctors <- n_nondoctors_on / n_nondoctors
        data.frame(
            ICD = group_label,
            n_doctors = n_doctors_on,
            prevalence_doctors = prevalence_doctors,
            n_nondoctors = n_nondoctors_on,
            prevalence_nondoctors = prevalence_nondoctors
        )
    })
    do.call(rbind, results)
}

# Define ICD prefixes
icd_prefixes <- list(
    "Type 2 Diabetes" = c("E11"),
    "Hypertension" = c("I10"),
    "Depression" = c("F32", "F33"),
    "COVID-19" = c("U07"),
    "Dyslipidemia" = c("E78"),
    "Myocardial Infarction" = c("I21", "I22"),
    "Stroke" = c("I61", "I63", "I64"),
    "ADHD" = c("F90"),
    "Epilepsy" = c("G40")
)

icd_prefixes <- list(
    "Type 2 Diabetes" = c("E11"),
    "Diabetes (all)" = c("E10", "E11", "E12", "E13", "E14"),
    "Hypertension" = c("I10"),
    "Ischaemic Heart Disease (all)" = c("I10", "I11", "I12", "I13", "I14", "I15"),
    "HIV/AIDS" = c("B20", "B21", "B22", "B23", "B24"),
    "Asthma" = c("J45"),
    "COPD" = c("J44"),
    "Cancer (any)" = c("C"),
    "Injusries/Accidents" = c("V", "X"),
    "Parkinson's Disease" = c("G20"),
    "Depression" = c("F32", "F33"),
    "COVID-19" = c("U07"),
    "Dyslipidemia" = c("E78"),
    "Myocardial Infarction" = c("I21", "I22"),
    "Stroke" = c("I61", "I63", "I64"),
    "ADHD" = c("F90"),
    "Epilepsy" = c("G40")
)

icd_prevalences_matched_cohort <- get_icd_prevalence(icd_prefixes, diagnoses_cohort, N, N) %>%
    mutate(
        # ICD = factor(ICD, levels = icd_prefixes),
        prevalence_doctors = round(prevalence_doctors * 100, 2),
        prevalence_nondoctors = round(prevalence_nondoctors * 100, 2),
        factor_doctor_vs_nondoctor = ifelse(prevalence_nondoctors == 0, NA, round(prevalence_doctors / prevalence_nondoctors, 2))
    )

icd_prevalences_matched_cohort %>%
    gt() %>%
    tab_header(
        title = "ICD Prevalence: Doctors vs Non-Doctors (Matched Cohort)"
    ) %>%
    cols_label(
        ICD = "ICD Code",
        n_doctors = "Doctors (n)",
        prevalence_doctors = "Doctors (%)",
        n_nondoctors = "Non-Doctors (n)",
        prevalence_nondoctors = "Non-Doctors (%)",
        factor_doctor_vs_nondoctor = "Doctor/Non-Doctor Ratio"
    ) %>%
    fmt_number(
        columns = c(prevalence_doctors, prevalence_nondoctors, factor_doctor_vs_nondoctor),
        decimals = 2
    )