# Plot: What proportion of doctors are married to other doctors? And which specialties tend to marry to each other? (relative %)
# Identify doctors who are married to other doctors
# First, get a lookup table for doctor_id -> specialty
doctor_specialty_lookup <- doctor_characteristics %>%
    select(DOCTOR_ID, INTERPRETATION)

# Find all spouse relationships where both are in the doctor_IDs list
spouse_pairs <- doctors_family %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1, RELATIVE_TYPE == "SPOUSE", RELATIVE_ID %in% doctor_IDs$V1) %>%
    select(DOCTOR_ID, RELATIVE_ID)

# To avoid double-counting, keep only pairs where DOCTOR_ID < RELATIVE_ID
spouse_pairs_unique <- spouse_pairs %>%
    mutate(pair_id = pmap_chr(list(DOCTOR_ID, RELATIVE_ID), ~paste(sort(c(..1, ..2)), collapse = "_"))) %>%
    distinct(pair_id, .keep_all = TRUE)

# Proportion of doctors married to other doctors
doctors_with_doctor_spouse <- unique(c(spouse_pairs_unique$DOCTOR_ID, spouse_pairs_unique$RELATIVE_ID))
n_doctors_with_doctor_spouse <- length(doctors_with_doctor_spouse)
prop_doctors_with_doctor_spouse <- n_doctors_with_doctor_spouse / N_doctors * 100

doctor_doctor_marriage_status <- tibble(
    Status = c("Married to Doctor", "Not Married to Doctor"),
    n = c(n_doctors_with_doctor_spouse, N_doctors - n_doctors_with_doctor_spouse)
) %>%
    mutate(percentage = n / sum(n) * 100)

doctor_doctor_marriage_plot <- ggplot(doctor_doctor_marriage_status, aes(x = "", y = n, fill = Status)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("Married to Doctor" = "#1e90cf", "Not Married to Doctor" = "gray70")) +
    labs(title = "Proportion of Doctors Married to Other Doctors", fill = "Marriage Status") +
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
    file = file.path(plot_dir, paste0("doctor_doctor_marriage_plot_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
doctor_doctor_marriage_plot
dev.off()

# Specialty pairs: which specialties tend to marry each other?
# Join specialty info for both partners
spouse_specialties <- spouse_pairs_unique %>%
    left_join(doctor_specialty_lookup, by = c("DOCTOR_ID" = "DOCTOR_ID")) %>%
    rename(SPECIALTY_1 = INTERPRETATION) %>%
    left_join(doctor_specialty_lookup, by = c("RELATIVE_ID" = "DOCTOR_ID")) %>%
    rename(SPECIALTY_2 = INTERPRETATION) %>%
    filter(!is.na(SPECIALTY_1), !is.na(SPECIALTY_2))

# For symmetry, always order the specialties alphabetically in the pair
spouse_specialties <- spouse_specialties %>%
    mutate(
        SPEC_A = pmin(SPECIALTY_1, SPECIALTY_2),
        SPEC_B = pmax(SPECIALTY_1, SPECIALTY_2)
    )

specialty_pairs_count <- spouse_specialties %>%
    count(SPEC_A, SPEC_B, sort = TRUE) %>%
    mutate(percentage = n / sum(n) * 100)

# Plot top 15 most common specialty pairs
top_n <- 15
specialty_pairs_count_top <- specialty_pairs_count %>%
    slice_max(n, n = top_n)

specialty_pairs_plot <- ggplot(specialty_pairs_count_top, aes(x = n, y = reorder(paste(SPEC_A, "&", SPEC_B), n))) +
    geom_bar(stat = "identity", fill = "#1e90cf") +
    geom_text(aes(label = n), hjust = -0.1, size = 5) +
    labs(
        title = paste("Top", top_n, "Most Common Doctor-Doctor Specialty Pairs"),
        x = "Number of Married Pairs",
        y = "Specialty Pair"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))

# Save the plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("specialty_pairs_plot_", timestamp, ".pdf")),
    width = 14,
    height = 8
)
specialty_pairs_plot
dev.off()






# check distribution within matched cohort
ggplot(matched_df, aes(x = age, fill = factor(SEX, labels = c("Male", "Female")))) +
    geom_histogram(binwidth = 1, position = "dodge", color = "black") +
    facet_wrap(~ is_doctor, labeller = as_labeller(c(`0` = "Not Doctor", `1` = "Doctor"))) +
    scale_fill_manual(values = c("blue", "pink")) +
    labs(title = "Age Distribution by Sex: Doctors vs Not Doctors",
             x = "Age",
             y = "Count",
             fill = "Sex") +
    theme_minimal()





# Create a blue gradient palette based on the number of doctors
prescription_boxplot <- ggplot(prescription_counts_filtered, aes(x = factor(YEAR), y = COUNT, fill = n_doctors)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
    # Add average as a point (mean per year)
    stat_summary(
        fun = mean,
        geom = "point",
        shape = 23,
        size = 3,
        color = "red",
        fill = "white"
    ) +
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
    )



prescription_counts <- prescription_counts %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    mutate(YEAR = as.numeric(YEAR)) %>%
    group_by(DOCTOR_ID, YEAR) %>%
    summarize(COUNT = sum(COUNT, na.rm = TRUE), .groups = "drop")



# Plot: What is the distribution of prescription counts per doctor by year by specialty?
# Add specialty information to prescription counts
prescription_counts_with_spec <- prescription_counts_filtered %>%
    left_join(doctor_characteristics %>% select(DOCTOR_ID, INTERPRETATION), by = "DOCTOR_ID") %>%
    rename(SPECIALTY = INTERPRETATION) %>%
    # filter(!is.na(SPECIALTY) & SPECIALTY != "") %>% 
    mutate(SPECIALTY = ifelse(is.na(SPECIALTY) | SPECIALTY == "", "NO SPECIALTY", SPECIALTY)) 

# Create a boxplot for each specialty by year
prescription_boxplot_by_spec <- ggplot(prescription_counts_with_spec, aes(x = factor(YEAR), y = COUNT, fill = SPECIALTY)) +
    geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
    scale_fill_brewer(palette = "Set3") +  # Use a qualitative palette for specialties
    labs(
        title = "Distribution of Prescription Counts per Doctor by Year and Specialty",
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