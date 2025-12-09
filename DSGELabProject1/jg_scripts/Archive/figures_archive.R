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




enrichment_df <- marriage_matrix %>%
    group_by(SPECIALTY_DOCTOR) %>%
    summarize(
        n_within = n[SPECIALTY_DOCTOR == SPECIALTY_SPOUSE],
        n_total = sum(n),
        prob_within = ifelse(n_total > 0, n_within / n_total, NA_real_),
        prob_between = ifelse(n_total > 0, 1 - prob_within, NA_real_)
    ) %>%
    mutate(
        factor_between_within = prob_between / prob_within,
        # Binomial 95% CI for prob_within
        se_within = sqrt(prob_within * (1 - prob_within) / n_total),
        ci_within_low = prob_within - 1.96 * se_within,
        ci_within_high = prob_within + 1.96 * se_within,
        # Binomial 95% CI for prob_between
        se_between = sqrt(prob_between * (1 - prob_between) / n_total),
        ci_between_low = prob_between - 1.96 * se_between,
        ci_between_high = prob_between + 1.96 * se_between,
        # Delta method for ratio CI (approximate)
        se_ratio = factor_between_within * sqrt(
            (se_between / prob_between)^2 + (se_within / prob_within)^2
        ),
        ci_ratio_low = factor_between_within - 1.96 * se_ratio,
        ci_ratio_high = factor_between_within + 1.96 * se_ratio
    ) %>%
    filter(n_total >= 10) %>% # privacy: only show specialties with >=10 marriages
    arrange(desc(factor_between_within))

# Plot: Factor of probability of marrying between specialty compared to within specialty (with 95% CI)
factor_plot <- ggplot(enrichment_df, aes(x = factor_between_within, y = reorder(SPECIALTY_DOCTOR, factor_between_within))) +
    geom_bar(stat = "identity", fill = "#7570b3") +
    geom_errorbarh(aes(xmin = pmax(0, ci_ratio_low), xmax = ci_ratio_high), height = 0.3, color = "black") +
    geom_text(aes(label = sprintf("%.2fx", factor_between_within)), hjust = -0.1, size = 4) +
    labs(
        title = "Relative Probability: Marrying Between vs Within Specialty",
        x = "Factor (Between / Within Probability)",
        y = "Specialty"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))




enrichment_df <- marriage_matrix %>%
    group_by(SPECIALTY_DOCTOR) %>%
    summarize(
        prob_within = prob[SPECIALTY_DOCTOR == SPECIALTY_SPOUSE],
        prob_between = 1 - prob_within,
        enrichment = prob_within / prob_between,
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
        x = "Enrichment Ratio (Within / Between)",
        y = "Specialty"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.1)))



married_status <- doctors_family %>%
    filter(DOCTOR_ID %in% doctor_IDs$V1) %>%
    group_by(DOCTOR_ID) %>%
    summarize(is_married = any(RELATIVE_TYPE == "SPOUSE")) %>%
    ungroup() %>%
    mutate(Marital_Status = ifelse(is_married, "Married", "Not Married")) %>%
    count(Marital_Status) %>% # add the number of IDs who are not in the relative df to not married and adjust N
    mutate(percentage = n / N_doctors * 100)



# Plot: Kaplan-Meier Survival Curves by Specialty (for specialties with at least 500 doctors) using facets, without survminer

# Prepare doctor-specialty lookup (non-empty specialties only)
doctor_specialty <- doctor_characteristics %>%
    filter(!is.na(INTERPRETATION) & INTERPRETATION != "") %>%
    select(DOCTOR_ID, SPECIALTY = INTERPRETATION)

# Add specialty to matched_df (only for doctors, non-doctors get NA)
matched_df <- matched_df %>%
    left_join(doctor_specialty, by = c("ID" = "DOCTOR_ID"))

# Count doctors per specialty (only doctors)
spec_counts <- matched_df %>%
    filter(is_doctor == 1, !is.na(SPECIALTY)) %>%
    count(SPECIALTY) %>%
    filter(n >= 500)

# For each specialty, keep only matched strata (age/sex) present in both groups and where the doctor has that specialty
# Only keep doctors with a valid specialty and their matched non-doctors (matched by SEX and age)
matched_df_spec <- matched_df %>%
    filter(
        (is_doctor == 1 & SPECIALTY %in% spec_counts$SPECIALTY) | is_doctor == 0
    ) %>%
    group_by(SEX, age) %>%
    filter(
        # Only keep strata where both doctor (with specialty) and non-doctor exist
        any(is_doctor == 1 & !is.na(SPECIALTY)) & any(is_doctor == 0)
    ) %>%
    ungroup()

# Only keep non-doctors that are matched to a doctor with a specialty (i.e., drop non-doctors in strata with no doctor with specialty)
matched_df_spec <- matched_df_spec %>%
    filter(is_doctor == 1 | (!is.na(SPECIALTY) & SPECIALTY %in% spec_counts$SPECIALTY))

# For each specialty, build KM for doctors of that specialty and their matched non-doctors
get_km_df <- function(df, specialty) {
    out <- list()
    for (g in 0:1) {
        fit <- survfit(Surv(age, DEATH) ~ 1, data = df[df$is_doctor == g, ])
        km_df <- data.frame(
            time = fit$time,
            surv = fit$surv,
            lower = fit$lower,
            upper = fit$upper,
            Group = ifelse(g == 1, "Doctor", "Not Doctor"),
            SPECIALTY = specialty
        )
        out[[length(out) + 1]] <- km_df
    }
    bind_rows(out)
}

km_df_spec <- matched_df_spec %>%
    filter(!is.na(SPECIALTY)) %>%
    group_by(SPECIALTY) %>%
    group_modify(~ get_km_df(.x, .y$SPECIALTY[1])) %>%
    ungroup()

# Plot with facets by specialty
km_plot_spec <- ggplot(km_df_spec, aes(x = time, y = surv, color = Group, fill = Group)) +
    geom_step(size = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    facet_wrap(~ SPECIALTY, scales = "free_y") +
    labs(
        title = "Kaplan-Meier Survival Curves by Specialty (≥500 doctors)",
        x = "Age",
        y = "Survival Probability",
        color = "Group",
        fill = "Group"
    ) +
    scale_color_manual(values = c("Not Doctor" = "gray70", "Doctor" = "steelblue")) +
    scale_fill_manual(values = c("Not Doctor" = "gray70", "Doctor" = "steelblue")) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(size = 11)
    )

# Save the faceted plot as a pdf file
pdf(
    file = file.path(plot_dir, paste0("km_plot_doctor_vs_nondoctor_by_specialty_", timestamp, ".pdf")),
    width = 16,
    height = 10
)
print(km_plot_spec)
dev.off()






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

# Plot with ggplot2, faceted by specialty
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
    )




# Add a risk table underneath the KM plot (percentage alive at each timepoint per specialty)
    # We'll use survminer::ggsurvplot for this, as it supports risk tables natively

    if (!requireNamespace("survminer", quietly = TRUE)) {
        install.packages("survminer")
    }
    library(survminer)

    # Prepare data: doctors only, large specialties
    km_spec_all_fit <- survfit(Surv(age, DEATH) ~ SPECIALTY, data = matched_df %>% filter(SPECIALTY %in% large_specialties, is_doctor == 1))

    # Plot with risk table (percentage alive)
    km_spec_all_survminer <- ggsurvplot(
        km_spec_all_fit,
        data = matched_df %>% filter(SPECIALTY %in% large_specialties, is_doctor == 1),
        risk.table = TRUE,
        risk.table.title = "Percentage Alive",
        risk.table.height = 0.25,
        risk.table.y.text.col = TRUE,
        risk.table.y.text = FALSE,
        palette = spec_colors,
        legend.title = "Specialty",
        legend.labs = large_specialties,
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



# Optionally: Fit and print Cox model for each specialty
cox_results <- lapply(large_specialties, function(spec) {
    dat <- matched_df_large_spec %>% filter(SPECIALTY == spec)
    if (nrow(dat) > 0 && length(unique(dat$is_doctor)) == 2) {
        model <- coxph(Surv(age, DEATH) ~ is_doctor, data = dat)
        summary(model)
    } else {
        NULL
    }
})
names(cox_results) <- large_specialties



# Optionally: Fit and print Cox model for each specialty using gtsummary
cox_gtsummary_list <- lapply(large_specialties, function(spec) {
    dat <- matched_df_large_spec %>% filter(SPECIALTY == spec)
    if (nrow(dat) > 0 && length(unique(dat$is_doctor)) == 2) {
        model <- coxph(Surv(age, DEATH) ~ is_doctor, data = dat)
        tbl_regression(model, exponentiate = TRUE) %>%
            modify_header(label = paste0("Specialty: ", spec, " - Variable")) %>%
            modify_caption(paste0("Cox Model for ", spec))
    } else {
        NULL
    }
})
names(cox_gtsummary_list) <- large_specialties

# Example: print the first table
# print(cox_gtsummary_list[[1]])


# Save the Cox model summary table as a PDF using grid graphics (no Chrome/webshot required)
cox_stats_combined_df <- as.data.frame(cox_stats_combined)
pdf(
    file = file.path(plot_dir, paste0("cox_model_by_specialty_", timestamp, ".pdf")),
    width = 10,
    height = 8
)
grid.newpage()
grid.draw(gridExtra::tableGrob(cox_stats_combined_df, rows = NULL))
dev.off()



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

prescription_counts_years <- prescription_counts %>%
    left_join(doctor_characteristics %>% select(DOCTOR_ID, START_YEAR), by = "DOCTOR_ID") %>%
    mutate(practicing_year = YEAR - START_YEAR + 1) %>%
    filter(practicing_year > 0) %>% # QC
    group_by(practicing_year) %>% 
    summarize(
        COUNT = sum(COUNT, na.rm = TRUE),
        n_doctors = n_distinct(DOCTOR_ID),
        avg_count = mean(COUNT, na.rm = TRUE),
        sd_count = ifelse(n() > 1, sd(COUNT, na.rm = TRUE), NA_real_),
        se = ifelse(n() > 1, sd(COUNT, na.rm = TRUE) / sqrt(n()), NA_real_),
        ci_lower = ifelse(n() > 1, avg_count - qt(0.975, df = n() - 1) * (sd(COUNT, na.rm = TRUE) / sqrt(n())), NA_real_),
        ci_upper = ifelse(n() > 1, avg_count + qt(0.975, df = n() - 1) * (sd(COUNT, na.rm = TRUE) / sqrt(n())), NA_real_),
        median = median(COUNT, na.rm = TRUE),
        perc_25 = quantile(COUNT, 0.25, na.rm = TRUE),
        perc_75 = quantile(COUNT, 0.75, na.rm = TRUE)
    ) %>%
    ungroup()



prescription_counts_years_combined_plot <- prescription_counts_years %>%
    filter(practicing_year >= 1, practicing_year <= 30) %>%
    ggplot(aes(x = practicing_year)) +
    # Median and IQR (blue)
    geom_line(aes(y = median, color = "Median"), size = 1.2) +
    geom_ribbon(aes(ymin = perc_25, ymax = perc_75, fill = "IQR"), alpha = 0.18) +
    # Mean and 95% CI (orange)
    geom_line(aes(y = avg_count, color = "Mean"), size = 1.2) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = "95% CI"), alpha = 0.18) +
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



dp_summary_stats_by_spec <- dp_summary %>%
    filter(!is.na(SPECIALTY) & SPECIALTY != "") %>%
    group_by(practicing_year, SPECIALTY) %>%
    summarize(
        n_doctors = n(),
        avg_total_patients = mean(TotalPatients, na.rm = TRUE),
        sd_total_patients = sd(TotalPatients, na.rm = TRUE),
        median_total_patients = median(TotalPatients, na.rm = TRUE),
        perc25_total_patients = quantile(TotalPatients, 0.25, na.rm = TRUE),
        perc75_total_patients = quantile(TotalPatients, 0.75, na.rm = TRUE),
        avg_unique_patients = mean(UniquePatients, na.rm = TRUE),
        sd_unique_patients = sd(UniquePatients, na.rm = TRUE),
        median_unique_patients = median(UniquePatients, na.rm = TRUE),
        perc25_unique_patients = quantile(UniquePatients, 0.25, na.rm = TRUE),
        perc75_unique_patients = quantile(UniquePatients, 0.75, na.rm = TRUE),
        avg_total_visits = mean(TotalVisits, na.rm = TRUE),
        sd_total_visits = sd(TotalVisits, na.rm = TRUE),
        median_total_visits = median(TotalVisits, na.rm = TRUE),
        perc25_total_visits = quantile(TotalVisits, 0.25, na.rm = TRUE),
        perc75_total_visits = quantile(TotalVisits, 0.75, na.rm = TRUE),
        avg_randomization_fct = mean(randomization_fct, na.rm = TRUE),
        sd_randomization_fct = sd(randomization_fct, na.rm = TRUE),
        median_randomization_fct = median(randomization_fct, na.rm = TRUE),
        perc25_randomization_fct = quantile(randomization_fct, 0.25, na.rm = TRUE),
        perc75_randomization_fct = quantile(randomization_fct, 0.75, na.rm = TRUE)
    ) %>%
    ungroup()



dp_summary_stats_by_spec_plot <- dp_summary_stats_by_spec %>%
    filter(practicing_year >= 1, practicing_year <= 30) %>%
    ggplot(aes(x = practicing_year)) +
    # 95% CI ribbon (orange)
    geom_ribbon(aes(ymin = ci_lower_randomization_fct, ymax = ci_upper_randomization_fct, fill = "95% CI"), alpha = 0.35) +
    # IQR ribbon (blue)
    geom_ribbon(aes(ymin = perc25_randomization_fct, ymax = perc75_randomization_fct, fill = "IQR"), alpha = 0.18) +
    # Median and Mean lines
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
        title = "Doctor-Patient Randomization Factor per Practicing Year by Specialty",
        x = "Practicing Year",
        y = "Randomization Factor (UniquePatients / TotalVisits)"
    ) +
    facet_wrap(~ SPECIALTY, ncol = 2, scales = "free_y") +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        strip.text = element_text(size = 13)
    )



dp_summary_stats_by_spec_grouped <- dp_summary_stats_by_spec %>%
    group_by(practicing_year, SPECIALTY_GROUP) %>%
    summarize(
        n_doctors = sum(n_doctors),
        avg_randomization_fct = mean(avg_randomization_fct, na.rm = TRUE),
        sd_randomization_fct = sd(avg_randomization_fct, na.rm = TRUE),
        se_randomization_fct = sd_randomization_fct / sqrt(n_doctors),
        ci_lower_randomization_fct = avg_randomization_fct - qt(0.975, df = n_doctors - 1) * se_randomization_fct,
        ci_upper_randomization_fct = avg_randomization_fct + qt(0.975, df = n_doctors - 1) * se_randomization_fct,
        median_randomization_fct = median(median_randomization_fct, na.rm = TRUE),
        perc25_randomization_fct = median(perc25_randomization_fct, na.rm = TRUE),
        perc75_randomization_fct = median(perc75_randomization_fct, na.rm = TRUE)
    ) %>%
    ungroup()







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


pd_summary_stats_by_age_sex_plot <- pd_summary_stats_by_age_sex %>%
    filter(age >= 0, age <= 100) %>%
    mutate(SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))) %>%
    ggplot(aes(x = age, group = SEX, color = SEX, fill = SEX)) +
    geom_ribbon(aes(ymin = ci_lower_randomization_fct, ymax = ci_upper_randomization_fct), alpha = 0.18, color = NA) +
    geom_ribbon(aes(ymin = perc25_randomization_fct, ymax = perc75_randomization_fct), alpha = 0.10, color = NA) +
    geom_line(aes(y = median_randomization_fct), size = 1.2) +
    geom_line(aes(y = avg_randomization_fct), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Male" = "blue", "Female" = "pink")) +
    scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
    labs(
        title = "Patient-Doctor Randomization Factor per Age by Sex",
        x = "Age",
        y = "Randomization Factor (UniqueDoctors / TotalVisits)",
        color = "Sex",
        fill = "Sex"
    ) +
    coord_cartesian(ylim = c(0.3, 1.0)) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom"
    )






dp_summary_selfpresc %>%
    filter(n_doctors > 0) %>%
    mutate(
        highlight = perc_selfprescribing == min(perc_selfprescribing, na.rm = TRUE)
    ) %>%
    ggplot(aes(x = reorder(SPECIALTY, perc_selfprescribing), y = perc_selfprescribing)) +
    geom_segment(aes(xend = SPECIALTY, y = 93, yend = perc_selfprescribing, color = highlight), size = 1.2) +
    geom_point(aes(color = highlight), size = 4) +
    geom_text(aes(label = sprintf("%.1f%%", perc_selfprescribing)), 
              hjust = -0.1, vjust = 0.5, size = 4, color = "black") +
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




    # Calculate number of doctors and nondoctors on medications starting with A10 (antidiabetics)
n_doctors_a10 <- prescriptions %>%
    filter(is_doctor & grepl("^A10", CODE)) %>%
    distinct(PATIENT_ID) %>%
    nrow()

n_notdoctors_a10 <- prescriptions %>%
    filter(!is_doctor & grepl("^A10", CODE)) %>%
    distinct(PATIENT_ID) %>%
    nrow()

# Calculate prevalence
prevalence_doctors_a10 <- n_doctors_a10 / n_doctors
prevalence_notdoctors_a10 <- n_notdoctors_a10 / n_notdoctors




# Calculate the number of doctors and non-doctors
n_doctors = nrow(doctor_IDs)
n_notdoctors <- dvv %>%
    filter(!(ID %in% doctor_IDs$V1)) %>%
    # should be alive in 2003 to match with doctors
    filter(is.na(DEATH_DATE) | DEATH_DATE > as.Date("2003-01-01")) %>%
    distinct(ID) %>%
    nrow()


# Get ATC prevalences
get_atc_prevalence <- function(atc_prefixes, prescriptions, n_doctors, n_nondoctors) {
    results <- lapply(atc_prefixes, function(prefix) {
        # Doctors
        n_doctors_on <- prescriptions %>%
            filter(is_doctor == TRUE, grepl(paste0("^", prefix), CODE)) %>%
            distinct(PATIENT_ID) %>%
            nrow()
        # Non-doctors
        n_nondoctors_on <- prescriptions %>%
            filter(is_doctor == FALSE, grepl(paste0("^", prefix), CODE)) %>%
            distinct(PATIENT_ID) %>%
            nrow()
        # Prevalence
        prevalence_doctors <- n_doctors_on / n_doctors
        prevalence_nondoctors <- n_nondoctors_on / n_nondoctors
        data.frame(
            ATC = prefix,
            n_doctors = n_doctors_on,
            prevalence_doctors = prevalence_doctors,
            n_nondoctors = n_nondoctors_on,
            prevalence_nondoctors = prevalence_nondoctors
        )
    })
    do.call(rbind, results)
}

# Define ATC prefixes
atc_prefixes <- c("A10", "C02", "N06A", "N05", "C10AA")

get_atc_prevalence(atc_prefixes, prescriptions, n_doctors, n_notdoctors) %>%
    mutate(
        ATC = factor(ATC, levels = atc_prefixes),
        prevalence_doctors = round(prevalence_doctors * 100, 2),
        prevalence_nondoctors = round(prevalence_nondoctors * 100, 2)
    ) -> atc_prevalences



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

atc_prevalences_matched_cohort <- get_atc_prevalence(atc_prefixes, prescriptions_cohort, N, N) %>%
    mutate(
        ATC = factor(ATC, levels = atc_prefixes),
        prevalence_doctors = round(prevalence_doctors * 100, 2),
        prevalence_nondoctors = round(prevalence_nondoctors * 100, 2)
    ) 


# Plot: Violin Plot of Total Visits by practicing_years by Sex using dp_summary_stats_total_summary
dp_summary_stats_total_summary  %>%
    filter(n >= 5) %>%
    ggplot(aes(x = factor(practicing_years), y = mean_TotalVisits, fill = SEX)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_point(position = position_jitter(width = 0.2), size = 1.5, color = "black") +
    labs(
        title = "Total Visits by Practicing Years and Sex",
        x = "Practicing Years",
        y = "Total Visits"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom"
    )



# Split data by sex
male_visits <- dp_summary_stats_total_summary %>% filter(SEX == "Male") %>% pull(mean_TotalVisits)
female_visits <- dp_summary_stats_total_summary %>% filter(SEX == "Female") %>% pull(mean_TotalVisits)
plot_df <- tibble(
    value = c(male_visits, female_visits),
    SEX = rep(c("Male", "Female"), c(length(male_visits), length(female_visits)))
)

# Custom function for half-violin using stat_density
half_violin <- function(data, sex, fill, side = "left", ...) {
    dens <- density(data$value, na.rm = TRUE)
    x <- dens$y / max(dens$y) * 0.4 # scale width
    y <- dens$x
    if (side == "left") {
        df <- data.frame(x = 1 - x, y = y)
    } else {
        df <- data.frame(x = 1 + x, y = y)
    }
    geom_polygon(
        data = df,
        aes(x = x, y = y),
        fill = fill,
        alpha = 0.7,
        ...
    )
}

# Plot: Half-violin for Male (left) and Female (right)
ggplot() +
    half_violin(plot_df %>% filter(SEX == "Male"), "Male", fill = "steelblue", side = "left") +
    half_violin(plot_df %>% filter(SEX == "Female"), "Female", fill = "pink", side = "right") +
    geom_boxplot(
        data = plot_df,
        aes(x = 1, y = value, fill = SEX),
        width = 0.12,
        outlier.shape = NA,
        color = "black",
        position = position_nudge(x = 0)
    ) +
    geom_jitter(
        data = plot_df,
        aes(x = 1, y = value, color = SEX),
        width = 0.08,
        alpha = 0.5,
        size = 2
    ) +
    scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
    scale_color_manual(values = c("Male" = "steelblue", "Female" = "pink")) +
    labs(
        title = "Distribution of Mean Total Visits by Sex (Half-Violin Plot)",
        x = "Sex",
        y = "Mean Total Visits"
    ) +
    scale_x_continuous(
        breaks = 1,
        labels = "Male (left) / Female (right)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none"
    )



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

dp_summary_stats_total_summary <- dp_summary_stats_total %>%
    group_by(SEX) %>%
    summarize(
        n = n(),
        mean_VisitsPerYear = mean(VisitsPerYear, na.rm = TRUE),
        sd_VisitsPerYear = sd(VisitsPerYear, na.rm = TRUE),
        se_VisitsPerYear = sd_VisitsPerYear / sqrt(n),
        ci_lower_VisitsPerYear = mean_VisitsPerYear - qt(0.975, df = n - 1) * se_VisitsPerYear,
        ci_upper_VisitsPerYear = mean_VisitsPerYear + qt(0.975, df = n - 1) * se_VisitsPerYear,
        median_VisitsPerYear = median(VisitsPerYear, na.rm = TRUE),
        perc25_VisitsPerYear = quantile(VisitsPerYear, 0.25, na.rm = TRUE),
        perc75_VisitsPerYear = quantile(VisitsPerYear, 0.75, na.rm = TRUE),

        mean_PatientsPerYear = mean(PatientsPerYear, na.rm = TRUE),
        sd_PatientsPerYear = sd(PatientsPerYear, na.rm = TRUE),
        se_PatientsPerYear = sd_PatientsPerYear / sqrt(n),
        ci_lower_PatientsPerYear = mean_PatientsPerYear - qt(0.975, df = n - 1) * se_PatientsPerYear,
        ci_upper_PatientsPerYear = mean_PatientsPerYear + qt(0.975, df = n - 1) * se_PatientsPerYear,
        median_PatientsPerYear = median(PatientsPerYear, na.rm = TRUE),
        perc25_PatientsPerYear = quantile(PatientsPerYear, 0.25, na.rm = TRUE),
        perc75_PatientsPerYear = quantile(PatientsPerYear, 0.75, na.rm = TRUE),

        mean_UniquePatientsPerYear = mean(UniquePatientsPerYear, na.rm = TRUE),
        sd_UniquePatientsPerYear = sd(UniquePatientsPerYear, na.rm = TRUE),
        se_UniquePatientsPerYear = sd_UniquePatientsPerYear / sqrt(n),
        ci_lower_UniquePatientsPerYear = mean_UniquePatientsPerYear - qt(0.975, df = n - 1) * se_UniquePatientsPerYear,
        ci_upper_UniquePatientsPerYear = mean_UniquePatientsPerYear + qt(0.975, df = n - 1) * se_UniquePatientsPerYear,
        median_UniquePatientsPerYear = median(UniquePatientsPerYear, na.rm = TRUE),
        perc25_UniquePatientsPerYear = quantile(UniquePatientsPerYear, 0.25, na.rm = TRUE),
        perc75_UniquePatientsPerYear = quantile(UniquePatientsPerYear, 0.75, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(n >= 5) # QC: only include groups with at least 5 doctors





























# Create a publication-style gt table and save as HTML
table1_gt <- table1 %>%
    gt() %>%
    tab_header(
        title = md("**Table 1. Demographics and Baseline Characteristics of Doctors' Cohort**"),
        subtitle = md(paste0("N = ", n_doctors))
    ) %>%
    cols_label(
        Characteristic = "Characteristic",
        Value = "Value"
    ) %>%
    fmt_missing(columns = everything(), missing_text = "NA") %>%
    cols_align(columns = vars(Characteristic), align = "left") %>%
    cols_align(columns = vars(Value), align = "right") %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(everything())
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = vars(Characteristic))
    ) %>%
    tab_source_note(
        source_note = "Values are presented as mean (SD) or median (IQR) where appropriate. Percentages are over non-missing values."
    ) %>%
    tab_options(
        table.font.size = 12,
        heading.title.font.size = 14,
        heading.subtitle.font.size = 11,
        data_row.padding = px(4),
        row.striping.background_color = "#F7F7F7"
    )




# Build a compact table with indented specialties and percentages in Value
table1_base <- table1 %>% slice(1:6)

special_header <- tibble(Characteristic = "Top 5 Specialties (%)", Value = "")

indent <- "\u00A0\u00A0\u00A0"
special_rows <- top_specialties %>%
    transmute(
        Characteristic = paste0(indent, LAST_SPECIALTY),
        Value = paste0(n, " (", pct, "%)")
    )

table1_expanded <- bind_rows(table1_base, special_header, special_rows)

# Create and save gt table
table1_gt <- table1_expanded %>%
    gt() %>%
    tab_header(
        title = md("**Table 1. Demographics and Baseline Characteristics of Doctors' Cohort**"),
        subtitle = md(paste0("N = ", n_doctors))
    ) %>%
    cols_label(Characteristic = "Characteristic", Value = "Value") %>%
    fmt_missing(columns = everything(), missing_text = "") %>%
    cols_align(columns = vars(Characteristic), align = "left") %>%
    cols_align(columns = vars(Value), align = "right") %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything())) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = vars(Characteristic), rows = Characteristic == "Top 5 Specialties (%)")
    ) %>%
    tab_source_note(source_note = "Values are presented as mean (SD) or median (IQR) where appropriate. Percentages are over non-missing values.") %>%
    tab_options(table.font.size = 12, heading.title.font.size = 14, heading.subtitle.font.size = 11)






# Build table with indented specialties as separate rows and percentages in the Value column
table1_base <- table1 %>% slice(1:6)  # keep first 6 summary rows

# Header row for specialties
special_header <- tibble(
    Characteristic = "Top 5 Specialties (%)",
    Value = ""
)

# Indent using non-breaking spaces so the indentation is preserved in gt output
indent <- "\u00A0\u00A0\u00A0"

# Create rows for each top specialty with counts and percent in the Value column
special_rows <- tibble(
    Characteristic = paste0(indent, top_specialties$LAST_SPECIALTY),
    Value = paste0(top_specialties$n, " (", top_specialties$pct, "%)")
)

# Combine into final table
table1_expanded <- bind_rows(table1_base, special_header, special_rows)

# Create a publication-style gt table and save as HTML
table1_gt <- table1_expanded %>%
    gt() %>%
    tab_header(
        title = md("**Table 1. Demographics and Baseline Characteristics of Doctors' Cohort**"),
        subtitle = md(paste0("N = ", n_doctors))
    ) %>%
    cols_label(
        Characteristic = "Characteristic",
        Value = "Value"
    ) %>%
    fmt_missing(columns = everything(), missing_text = "") %>%
    cols_align(columns = vars(Characteristic), align = "left") %>%
    cols_align(columns = vars(Value), align = "right") %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(everything())
    ) %>%
    # Make the "Top 5 Specialties (%)" row bold to act as a subheading
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(
            columns = vars(Characteristic),
            rows = Characteristic == "Top 5 Specialties (%)"
        )
    ) %>%
    tab_source_note(
        source_note = "Values are presented as mean (SD) or median (IQR) where appropriate. Percentages are over non-missing values."
    ) %>%
    tab_options(
        table.font.size = 12,
        heading.title.font.size = 14,
        heading.subtitle.font.size = 11,
        data_row.padding = px(4),
        row.striping.background_color = "#F7F7F7"
    )




# Calculate Follow-up Time
# Convert PRACTICING_DAYS to follow-up time in years (2 decimals) and compute summary stats
doctor_characteristics <- doctor_characteristics %>%
    mutate(FOLLOWUP_YEARS = ifelse(is.na(PRACTICING_DAYS), NA, round(PRACTICING_DAYS / 365.25, 2)))

mean_followup <- round(mean(doctor_characteristics$FOLLOWUP_YEARS, na.rm = TRUE), 2)
sd_followup <- round(sd(doctor_characteristics$FOLLOWUP_YEARS, na.rm = TRUE), 2)
median_followup <- round(median(doctor_characteristics$FOLLOWUP_YEARS, na.rm = TRUE), 2)
iqr_followup <- round(IQR(doctor_characteristics$FOLLOWUP_YEARS, na.rm = TRUE), 2)




# Yearly Prescriptions
# Join prescriptions to doctors to get START_YEAR and compute practicing_year
prescription_counts <- prescription_counts %>%
    left_join(doctor_characteristics %>% select(DOCTOR_ID, START_YEAR), by = "DOCTOR_ID") %>%
    mutate(practicing_year = YEAR - START_YEAR + 1) %>%
    filter(!is.na(practicing_year) & practicing_year >= 1)

# If there are duplicate DOCTOR_ID/YEAR rows, sum them
prescription_counts <- prescription_counts %>%
    group_by(DOCTOR_ID, YEAR, practicing_year) %>%
    summarize(COUNT = sum(COUNT, na.rm = TRUE), .groups = "drop")

# Per-doctor summaries of yearly prescriptions (across observed years)
per_doctor_presc <- prescription_counts %>%
    group_by(DOCTOR_ID) %>%
    summarize(
        mean_yearly = mean(COUNT, na.rm = TRUE),
        sd_yearly = sd(COUNT, na.rm = TRUE),
        median_yearly = median(COUNT, na.rm = TRUE),
        iqr_yearly = IQR(COUNT, na.rm = TRUE),
        n_years = n(),
        .groups = "drop"
    )

# Overall summaries across doctors to use in Table 1
mean_yearly_prescriptions <- round(mean(per_doctor_presc$mean_yearly, na.rm = TRUE), 2)
sd_yearly_prescriptions   <- round(sd(per_doctor_presc$mean_yearly, na.rm = TRUE), 2)
median_yearly_prescriptions <- round(median(per_doctor_presc$mean_yearly, na.rm = TRUE), 2)
iqr_yearly_prescriptions  <- round(IQR(per_doctor_presc$mean_yearly, na.rm = TRUE), 2)

# Formatted string for insertion into the table (Mean (SD); Median (IQR))
yearly_prescriptions_value <- paste0(
    mean_yearly_prescriptions, " (", sd_yearly_prescriptions, "); ",
    median_yearly_prescriptions, " (", iqr_yearly_prescriptions, ")"
)





# THE FLAW HERE IS THAT THE prettyNum FUNCTION ONLY WORKS ON WHOLE NUMBERS
# SO IF THE LEADING NUMBER IS A DECIMAL, IT WILL NOT BE FORMATTED
# ALSO: IT DOESN'T HANDLE NEGATIVE NUMBERS
# MORE IMPORTANTLY: IT DOESN'T HANDLE NUMBERS AFTER THE LEADING NUMBER SO (SD/IQR) PARTS WILL BE AFFECTED
# FUNCTION to add commas to leading numbers in a string
add_commas_to_leading_number <- function(x) {
    out <- as.character(x)
    for (i in seq_along(out)) {
        m <- regexpr("^[0-9]+(?:\\.[0-9]+)?", out[i], perl = TRUE)
        if (m[1] != -1) {
            num <- regmatches(out[i], m)
            fmt <- prettyNum(as.numeric(num), big.mark = ",", scientific = FALSE, trim = TRUE)
            regmatches(out[i], m) <- fmt
        }
    }
    out
}

# apply comma formatting to the leading numbers in the Value column (e.g. "10000 (12.3%)" -> "10,000 (12.3%)")
table1_expanded$Value <- add_commas_to_leading_number(table1_expanded$Value)





################## Figure 2

# Preprocess the data
if (ncol(min_phen_diagnoses) < 6) stop("min_phen_diagnoses must have at least 6 columns")

data.table::setnames(
    min_phen_diagnoses,
    old = names(min_phen_diagnoses)[1:6],
    new = c("PATIENT_ID", "VISIT_DATE", "ICD10_CODE", "SOURCE", "FD_HASH_CODE", "DOCTOR_ID")
)

# Parse VISIT_DATE to Date (ISO YYYY-MM-DD); fall back to common dmy/mdy orders if needed
min_phen_diagnoses[, VISIT_DATE := as.IDate(VISIT_DATE, format = "%Y-%m-%d")]
min_phen_diagnoses[is.na(VISIT_DATE), VISIT_DATE := as.IDate(lubridate::parse_date_time(VISIT_DATE, orders = c("Ymd","dmy","mdy"), quiet = TRUE))]

# Ensure the canonical first-six-column order
data.table::setcolorder(min_phen_diagnoses, c("PATIENT_ID", "VISIT_DATE", "ICD10_CODE", "SOURCE", "FD_HASH_CODE", "DOCTOR_ID"))


all_diagnoses_wide <- dvv %>%
    filter(ID %in% min_phen_diagnoses$PATIENT_ID) %>%
    select(ID, SEX, BIRTH_DATE, DEATH_DATE) %>%
    left_join(min_phen_diagnoses, by = c("ID" = "PATIENT_ID")) %>%
    mutate(
        ICD10 = ifelse(!is.na(ICD10_CODE), 1, 0),
        ICD10_DATE = ifelse(!is.na(VISIT_DATE), VISIT_DATE, as.Date(NA)),
        ICD10_TIME = ifelse(!is.na(VISIT_DATE), as.numeric(difftime(VISIT_DATE, BIRTH_DATE, units = "days")), as.numeric(difftime(as.Date("2022-12-31"), BIRTH_DATE, units = "days")))
    ) 








# Build wide cohort df (min_phen_diagnoses already contains earliest diag per patient/ICD)
{
    # pivot to wide: columns like K21_DATE from VISIT_DATE (no summarise needed because earliest already present)
    diag_wide <- min_phen_diagnoses %>%
        select(PATIENT_ID, ICD10_CODE, VISIT_DATE) %>%
        tidyr::pivot_wider(
            id_cols = PATIENT_ID,
            names_from = ICD10_CODE,
            values_from = VISIT_DATE,
            names_glue = "{ICD10_CODE}_DATE"
        )

    # base population with follow-up window (1998-01-01 to min(DEATH_DATE, 2022-12-31))
    pop_base <- dvv %>%
        select(ID, SEX, BIRTH_DATE, DEATH_DATE) %>%
        mutate(
            FU_START = as.Date("1998-01-01"),
            FU_END = as.Date(ifelse(is.na(DEATH_DATE) | DEATH_DATE > as.Date("2022-12-31"),
                                    "2022-12-31", as.character(DEATH_DATE)))
        )

    # join and compute indicator/time columns
    cohort_diag_wide <- pop_base %>%
        left_join(diag_wide, by = c("ID" = "PATIENT_ID"))

    # find all *_DATE columns and create corresponding indicator and time columns
    date_cols <- grep("_DATE$", names(cohort_diag_wide), value = TRUE)

    for (dc in date_cols) {
        code <- sub("_DATE$", "", dc)
        ind_col <- code
        time_col <- paste0(code, "_TIME")

        cohort_diag_wide <- cohort_diag_wide %>%
            mutate(
                # ensure date is actual Date and only keep if within FU window
                !!dc := {
                    tmp <- .data[[dc]]
                    # try coercing to Date if not already
                    if (!inherits(tmp, "Date")) tmp <- as.Date(tmp)
                    ifelse(!is.na(tmp) & tmp >= FU_START & tmp <= FU_END, tmp, as.Date(NA))
                },
                # binary indicator: 1 if diagnosis within FU, else 0
                !!ind_col := ifelse(!is.na(.data[[dc]]), 1L, 0L),
                # time in years: if diagnosed -> (diag_date - FU_START) / 365.25; else -> (FU_END - FU_START) / 365.25
                !!time_col := dplyr::case_when(
                    is.na(FU_START) | is.na(FU_END) ~ NA_real_,
                    !is.na(.data[[dc]]) ~ as.numeric(difftime(.data[[dc]], FU_START, units = "days")) / 365.25,
                    TRUE ~ as.numeric(difftime(FU_END, FU_START, units = "days")) / 365.25
                )
            )
    }

    # keep only requested columns: ID, SEX, BIRTH_DATE, DEATH_DATE and the generated cols (sorted)
    gen_cols <- sort(c(date_cols, sub("_DATE$", "", date_cols), paste0(sub("_DATE$", "", date_cols), "_TIME")))
    keep_cols <- c("ID", "SEX", "BIRTH_DATE", "DEATH_DATE", gen_cols)
    cohort_diag_wide <- cohort_diag_wide %>% select(any_of(keep_cols))
}

# result: cohort_diag_wide



can you not only create the ICD10_CODE_DATE but also add the ICD10_CODE <1|0> if present of not

diag_wide <- min_phen_diagnoses %>%
    select(PATIENT_ID, ICD10_CODE, VISIT_DATE) %>%
    tidyr::pivot_wider(
        id_cols = PATIENT_ID,
        names_from = ICD10_CODE,
        values_from = VISIT_DATE,
        names_glue = "{ICD10_CODE}_DATE"
    ) %>%
    # add binary indicator (1/0) for each *_DATE column efficiently and interleave them next to the date columns
    {
      df <- .
      date_cols <- grep("_DATE$", names(df), value = TRUE)
      if (length(date_cols) == 0) {
        df
      } else {
        # create indicators for all *_DATE columns in one vectorized step (creates temporary *_DATE_IND names)
        df <- df %>%
          mutate(
            across(all_of(date_cols), ~ as.Date(.x), .names = "{.col}"),
            across(all_of(date_cols), ~ as.integer(!is.na(.x)), .names = "{.col}_IND")
          ) %>%
          # rename *_DATE_IND -> remove the _DATE_IND suffix so e.g. "K21_DATE_IND" -> "K21"
          rename_with(~ sub("_DATE_IND$", "", .x), ends_with("_DATE_IND"))
        # build desired interleaved column order: for each X_DATE place X immediately after it
        date_cols_new <- grep("_DATE$", names(df), value = TRUE)
        ind_cols <- sub("_DATE$", "", date_cols_new)
        pair_order <- unlist(lapply(date_cols_new, function(dc) c(dc, sub("_DATE$", "", dc))))
        # keep other columns in their original relative order, then put the date/indicator pairs
        other_cols <- setdiff(names(df), pair_order)
        df %>% select(any_of(other_cols), all_of(pair_order))
      }
    }



# Build wide cohort df (min_phen_diagnoses already contains earliest diag per patient/ICD)
{
    # pivot to wide: columns like K21_DATE from VISIT_DATE (no summarise needed because earliest already present)
    diag_wide <- min_phen_diagnoses %>%
        select(PATIENT_ID, ICD10_CODE, VISIT_DATE) %>%
        tidyr::pivot_wider(
            id_cols = PATIENT_ID,
            names_from = ICD10_CODE,
            values_from = VISIT_DATE,
            names_glue = "{ICD10_CODE}_DATE"
        )

    # base population with follow-up window (1998-01-01 to min(DEATH_DATE, 2022-12-31))
    pop_base <- dvv %>%
        select(ID, SEX, BIRTH_DATE, DEATH_DATE) %>%
        mutate(
            FU_START = as.Date("1998-01-01"),
            FU_END = as.Date(ifelse(is.na(DEATH_DATE) | DEATH_DATE > as.Date("2022-12-31"),
                                    "2022-12-31", as.character(DEATH_DATE)))
        )

    # join and compute indicator/time columns
    cohort_diag_wide <- pop_base %>%
        left_join(diag_wide, by = c("ID" = "PATIENT_ID"))

    # find all *_DATE columns and create corresponding indicator and time columns
    date_cols <- grep("_DATE$", names(cohort_diag_wide), value = TRUE)

    for (dc in date_cols) {
        code <- sub("_DATE$", "", dc)
        ind_col <- code
        time_col <- paste0(code, "_TIME")

        cohort_diag_wide <- cohort_diag_wide %>%
            mutate(
                # ensure date is actual Date and only keep if within FU window
                !!dc := {
                    tmp <- .data[[dc]]
                    # try coercing to Date if not already
                    if (!inherits(tmp, "Date")) tmp <- as.Date(tmp)
                    ifelse(!is.na(tmp) & tmp >= FU_START & tmp <= FU_END, tmp, as.Date(NA))
                },
                # binary indicator: 1 if diagnosis within FU, else 0
                !!ind_col := ifelse(!is.na(.data[[dc]]), 1L, 0L),
                # time in years: if diagnosed -> (diag_date - FU_START) / 365.25; else -> (FU_END - FU_START) / 365.25
                !!time_col := dplyr::case_when(
                    is.na(FU_START) | is.na(FU_END) ~ NA_real_,
                    !is.na(.data[[dc]]) ~ as.numeric(difftime(.data[[dc]], FU_START, units = "days")) / 365.25,
                    TRUE ~ as.numeric(difftime(FU_END, FU_START, units = "days")) / 365.25
                )
            )
    }

    # keep only requested columns: ID, SEX, BIRTH_DATE, DEATH_DATE and the generated cols (sorted)
    gen_cols <- sort(c(date_cols, sub("_DATE$", "", date_cols), paste0(sub("_DATE$", "", date_cols), "_TIME")))
    keep_cols <- c("ID", "SEX", "BIRTH_DATE", "DEATH_DATE", gen_cols)
    cohort_diag_wide <- cohort_diag_wide %>% select(any_of(keep_cols))
}

# result: cohort_diag_wide
    









# Test Example of age-sex adjusted incidence rate calculation

df <- diag_wide2 %>% 
    select(1:9) %>% 
    filter(is_doctor == 1)  %>%
    mutate(across(ends_with("_DATE"), ~ lubridate::ymd(na_if(as.character(.), "")))) 

END_OF_DATA <- as.Date("2022-12-31")

df2 <- df %>%
    mutate(
        ENTRY_DATE = pmax(as.Date("1998-01-01"), BIRTH_DATE, na.rm = TRUE),

        # If K21 == 1, use K21_DATE; otherwise set far future
        tmp_k21 = dplyr::if_else(K21 == 1, K21_DATE, as.Date("2100-01-01")),
        # If DEATH == 1, use DEATH_DATE; otherwise set far future
        tmp_death = dplyr::if_else(DEATH == 1, DEATH_DATE, as.Date("2100-01-01")),

        # EXIT is earliest of event, death, or censoring date
        EXIT_DATE = pmin(tmp_k21, tmp_death, END_OF_DATA, na.rm = TRUE),

        EVENT = as.integer(K21 == 1 & !is.na(K21_DATE) & K21_DATE <= EXIT_DATE)
    ) %>%
    select(-tmp_k21, -tmp_death)

# Build Lexis object
L <- Lexis(
  entry = list(
    age = as.numeric(ENTRY_DATE - BIRTH_DATE) / 365.25
  ),
  exit = list(
    age = as.numeric(EXIT_DATE - BIRTH_DATE) / 365.25
  ),
  exit.status = factor(EVENT, levels=c(0,1), labels=c("noK21","K21")),
  data = df2,
  id = ID
)

age_breaks <- seq(0, 100, by = 5)

L_split <- splitLexis(L,
                      breaks = list(age = age_breaks),
                      time.scale = "age")

L_tab <- L_split %>%
  mutate(
    age_band = cut(
      age,
      breaks = age_breaks,
      right = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  group_by(SEX, age_band) %>%
  summarise(
    pyrs   = sum(lex.dur),  # person-years in that sex × age-band
    events = sum(lex.Cst == "noK21" & lex.Xst == "K21"),
    .groups = "drop"
  ) %>%
  mutate(
    IR = events / pyrs,         # crude incidence rate for that stratum
    IR_1k = IR * 1000           # per 1,000 person-years (if you like)
  )

L_tab <- L_tab %>%
  mutate(weight = pyrs / sum(pyrs))   # weights over all age × sex strata

adj_IR   <- sum(L_tab$IR * L_tab$weight)
adj_IR_1k <- adj_IR * 1000   # per 1,000 PY
adj_IR_1k








































# Function to compute age- and sex-adjusted incidence (per 1,000 PY) for every ICD code
# Requires Epi and lubridate packages. Input diag_wide2 should be like in your script:
# - contains ID, SEX, BIRTH_DATE, DEATH_DATE, is_doctor (1/0) and for each ICD code columns: <ICD>_DATE (Date) and optionally <ICD> (0/1).
# Returns tibble with ICD_CODE and adj_IR_1k
compute_adj_ir_for_all_icd <- function(diag_wide2,
                                       icd_codes = NULL,
                                       start_date = as.Date("1998-01-01"),
                                       end_of_data = as.Date("2022-12-31"),
                                       age_breaks = seq(0, 100, by = 5),
                                       doctors_only = TRUE) {
  if (!requireNamespace("Epi", quietly = TRUE)) {
    stop("Package 'Epi' is required. Install it with install.packages('Epi').")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required. Install it with install.packages('lubridate').")
  }
  # prepare data copy
  d <- diag_wide2
  # parse important date columns
  d$BIRTH_DATE <- lubridate::ymd(as.character(d$BIRTH_DATE))
  d$DEATH_DATE <- lubridate::ymd(as.character(d$DEATH_DATE))
  # parse all *_DATE columns to Date (only ICD-related date columns starting from column 8)
  if (ncol(d) >= 8) {
    date_cols_all <- grep("_DATE$", names(d), value = TRUE)
    date_cols <- intersect(date_cols_all, names(d)[8:ncol(d)])
  } else {
    date_cols <- character(0)
  }
  # Exclude known non-ICD date columns
  date_cols <- setdiff(date_cols, c("DEATH_DATE", "DEATH"))
  for (dc in date_cols) {
    # convert empty strings to NA and parse to Date
    d[[dc]] <- lubridate::ymd(ifelse(as.character(d[[dc]]) == "", NA, as.character(d[[dc]])))
  }
  # filter to doctors if requested
  if (doctors_only && "is_doctor" %in% names(d)) d <- d[d$is_doctor == 1, , drop = FALSE]
  # determine ICD codes if not provided
  detected_icd_codes <- sub("_DATE$", "", date_cols)
  if (is.null(icd_codes)) {
    icd_codes <- detected_icd_codes
  } else {
    icd_codes <- intersect(icd_codes, detected_icd_codes)
  }
  if (length(icd_codes) == 0) {
    return(tibble::tibble(ICD_CODE = character(0), adj_IR_1k = numeric(0)))
  }
  results <- vector("list", length(icd_codes))
  names(results) <- icd_codes
  # constants
  FAR_FUTURE <- as.Date("2100-01-01")
  for (icd in icd_codes) {
    date_col <- paste0(icd, "_DATE")
    ind_col <- icd
    # extract vectors
    id_vec <- d$ID
    sex_vec <- d$SEX
    birth_vec <- d$BIRTH_DATE
    death_vec <- d$DEATH_DATE
    date_vec <- d[[date_col]]
    ind_vec <- if (ind_col %in% names(d)) d[[ind_col]] else as.integer(!is.na(date_vec))
    # ENTRY_DATE
    ENTRY_DATE <- pmax(start_date, birth_vec, na.rm = TRUE)
    # tmp event / death
    tmp_event <- ifelse(!is.na(date_vec), date_vec, FAR_FUTURE)
    tmp_death  <- ifelse(!is.na(death_vec), death_vec, FAR_FUTURE)
    # EXIT_DATE = earliest of event, death, censor date
    EXIT_DATE <- pmin(tmp_event, tmp_death, end_of_data, na.rm = TRUE)
    # EVENT indicator: require indicator==1 (if present) and date <= EXIT_DATE
    EVENT <- as.integer((!is.na(date_vec)) & (is.na(ind_vec) | ind_vec == 1) & (date_vec <= EXIT_DATE))
    # build analysis df
    df2 <- data.frame(
      ID = id_vec,
      SEX = sex_vec,
      BIRTH_DATE = birth_vec,
      ENTRY_DATE = ENTRY_DATE,
      EXIT_DATE = EXIT_DATE,
      EVENT = EVENT,
      stringsAsFactors = FALSE
    )
    # drop rows with missing needed dates
    keep <- !is.na(df2$BIRTH_DATE) & !is.na(df2$ENTRY_DATE) & !is.na(df2$EXIT_DATE)
    df2 <- df2[keep, , drop = FALSE]
    # if no person-time skip
    if (nrow(df2) == 0) {
      results[[icd]] <- NA_real_
      next
    }
    # make Lexis object and split by age
    # entry/exit ages in years
    entry_age <- as.numeric(df2$ENTRY_DATE - df2$BIRTH_DATE) / 365.25
    exit_age  <- as.numeric(df2$EXIT_DATE  - df2$BIRTH_DATE) / 365.25
    # build Lexis safely in tryCatch
    adj_ir_1k <- tryCatch({
      L <- Epi::Lexis(
        entry = list(age = entry_age),
        exit  = list(age = exit_age),
        exit.status = factor(df2$EVENT, levels = c(0,1), labels = c(paste0("no", icd), icd)),
        data = df2,
        id = df2$ID
      )
      L_split <- Epi::splitLexis(L, breaks = list(age = age_breaks), time.scale = "age")
      # compute pyrs and events by sex × age band
      L_tab <- as.data.frame(L_split)
      L_tab$age_band <- cut(L_tab$age, breaks = age_breaks, right = FALSE, include.lowest = TRUE)
      agg <- aggregate(cbind(pyrs = L_tab$lex.dur, events = as.integer(L_tab$lex.Cst == paste0("no", icd) & L_tab$lex.Xst == icd)),
                       by = list(SEX = L_tab$SEX, age_band = L_tab$age_band),
                       FUN = sum, na.rm = TRUE)
      # if no person-time, return NA
      total_pyrs <- sum(agg$pyrs, na.rm = TRUE)
      if (total_pyrs <= 0) return(NA_real_)
      agg$IR <- agg$events / agg$pyrs
      agg$weight <- agg$pyrs / total_pyrs
      adj_IR <- sum(agg$IR * agg$weight, na.rm = TRUE)
      adj_IR * 1000
    }, error = function(e) {
      NA_real_
    })
    results[[icd]] <- adj_ir_1k
  }
  # assemble tibble
  out <- tibble::tibble(ICD_CODE = names(results), adj_IR_1k = unlist(results, use.names = FALSE))
  out
}

# Example usage:
# res <- compute_adj_ir_for_all_icd(diag_wide2)
# head(res)




























# Function to compute age- and sex-adjusted incidence (per 1,000 PY) for every ICD code
# Requires Epi and lubridate packages. Input diag_wide2 should be like in your script:
# - contains ID, SEX, BIRTH_DATE, DEATH_DATE, is_doctor (1/0) and for each ICD code columns: <ICD>_DATE (Date) and optionally <ICD> (0/1).
# Returns tibble with ICD_CODE and adj_IR_1k
compute_adj_ir_for_all_icd <- function(diag_wide2,
                                       icd_codes = NULL,
                                       start_date = as.Date("1998-01-01"),
                                       end_of_data = as.Date("2022-12-31"),
                                       age_breaks = seq(0, 100, by = 5),
                                       doctors_only = TRUE) {
  if (!requireNamespace("Epi", quietly = TRUE)) {
    stop("Package 'Epi' is required. Install it with install.packages('Epi').")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required. Install it with install.packages('lubridate').")
  }
  # prepare data copy
  d <- diag_wide2
  # parse important date columns
  d$BIRTH_DATE <- lubridate::ymd(as.character(d$BIRTH_DATE))
  d$DEATH_DATE <- lubridate::ymd(as.character(d$DEATH_DATE))
  # parse all *_DATE columns to Date
  date_cols <- grep("_DATE$", names(d), value = TRUE)
  for (dc in date_cols) d[[dc]] <- lubridate::ymd(na_if(as.character(d[[dc]]), ""))
  # filter to doctors if requested
  if (doctors_only && "is_doctor" %in% names(d)) d <- d[d$is_doctor == 1, , drop = FALSE]
  # determine ICD codes if not provided
  detected_icd_codes <- sub("_DATE$", "", date_cols)
  if (is.null(icd_codes)) {
    icd_codes <- detected_icd_codes
  } else {
    icd_codes <- intersect(icd_codes, detected_icd_codes)
  }
  if (length(icd_codes) == 0) {
    return(tibble::tibble(ICD_CODE = character(0), adj_IR_1k = numeric(0)))
  }
  results <- vector("list", length(icd_codes))
  names(results) <- icd_codes
  # constants
  FAR_FUTURE <- as.Date("2100-01-01")
  for (icd in icd_codes) {
    date_col <- paste0(icd, "_DATE")
    ind_col <- icd
    # extract vectors
    id_vec <- d$ID
    sex_vec <- d$SEX
    birth_vec <- d$BIRTH_DATE
    death_vec <- d$DEATH_DATE
    date_vec <- d[[date_col]]
    ind_vec <- if (ind_col %in% names(d)) d[[ind_col]] else as.integer(!is.na(date_vec))
    # ENTRY_DATE
    ENTRY_DATE <- pmax(start_date, birth_vec, na.rm = TRUE)
    # tmp event / death
    tmp_event <- ifelse(!is.na(date_vec), date_vec, FAR_FUTURE)
    tmp_death  <- ifelse(!is.na(death_vec), death_vec, FAR_FUTURE)
    # EXIT_DATE = earliest of event, death, censor date
    EXIT_DATE <- pmin(tmp_event, tmp_death, end_of_data, na.rm = TRUE)
    # EVENT indicator: require indicator==1 (if present) and date <= EXIT_DATE
    EVENT <- as.integer((!is.na(date_vec)) & (is.na(ind_vec) | ind_vec == 1) & (date_vec <= EXIT_DATE))
    # build analysis df
    df2 <- data.frame(
      ID = id_vec,
      SEX = sex_vec,
      BIRTH_DATE = birth_vec,
      ENTRY_DATE = ENTRY_DATE,
      EXIT_DATE = EXIT_DATE,
      EVENT = EVENT,
      stringsAsFactors = FALSE
    )
    # drop rows with missing needed dates
    keep <- !is.na(df2$BIRTH_DATE) & !is.na(df2$ENTRY_DATE) & !is.na(df2$EXIT_DATE)
    df2 <- df2[keep, , drop = FALSE]
    # if no person-time skip
    if (nrow(df2) == 0) {
      results[[icd]] <- NA_real_
      next
    }
    # make Lexis object and split by age
    # entry/exit ages in years
    entry_age <- as.numeric(df2$ENTRY_DATE - df2$BIRTH_DATE) / 365.25
    exit_age  <- as.numeric(df2$EXIT_DATE  - df2$BIRTH_DATE) / 365.25
    # build Lexis safely in tryCatch
    adj_ir_1k <- tryCatch({
      L <- Epi::Lexis(
        entry = list(age = entry_age),
        exit  = list(age = exit_age),
        exit.status = factor(df2$EVENT, levels = c(0,1), labels = c(paste0("no", icd), icd)),
        data = df2,
        id = df2$ID
      )
      L_split <- Epi::splitLexis(L, breaks = list(age = age_breaks), time.scale = "age")
      # compute pyrs and events by sex × age band
      L_tab <- as.data.frame(L_split)
      L_tab$age_band <- cut(L_tab$age, breaks = age_breaks, right = FALSE, include.lowest = TRUE)
      agg <- aggregate(cbind(pyrs = L_tab$lex.dur, events = as.integer(L_tab$lex.Cst == paste0("no", icd) & L_tab$lex.Xst == icd)),
                       by = list(SEX = L_tab$SEX, age_band = L_tab$age_band),
                       FUN = sum, na.rm = TRUE)
      # if no person-time, return NA
      total_pyrs <- sum(agg$pyrs, na.rm = TRUE)
      if (total_pyrs <= 0) return(NA_real_)
      agg$IR <- agg$events / agg$pyrs
      agg$weight <- agg$pyrs / total_pyrs
      adj_IR <- sum(agg$IR * agg$weight, na.rm = TRUE)
      adj_IR * 1000
    }, error = function(e) {
      NA_real_
    })
    results[[icd]] <- adj_ir_1k
  }
  # assemble tibble
  out <- tibble::tibble(ICD_CODE = names(results), adj_IR_1k = unlist(results, use.names = FALSE))
  out
}

# Example usage:
# res <- compute_adj_ir_for_all_icd(diag_wide2)
# head(res)


















calculate_all_incidence_rates <- function(diag_wide, end_of_data = as.Date("2022-12-31")) {
  
  # Filter to doctors only and prepare base data
  df_base <- diag_wide %>% 
    filter(is_doctor == 1) %>%
    select(ID, SEX, BIRTH_DATE, DEATH_DATE, DEATH) %>%
    mutate(
      BIRTH_DATE = ymd(na_if(as.character(BIRTH_DATE), "")),
      DEATH_DATE = ymd(na_if(as.character(DEATH_DATE), ""))
    )
  
  # Identify all ICD code columns (those with corresponding _DATE columns)
  all_cols <- names(diag_wide)
  date_cols <- all_cols[grepl("_DATE$", all_cols) & 
                        !all_cols %in% c("BIRTH_DATE", "DEATH_DATE")]
  
  # Extract ICD codes (remove _DATE suffix)
  icd_codes <- sub("_DATE$", "", date_cols)
  
  # Initialize results data frame
  results <- data.frame(
    ICD_CODE = character(),
    adj_IR_1k = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each ICD code
  for (icd in icd_codes) {
    
    tryCatch({
      date_col <- paste0(icd, "_DATE")
      
      # Create working dataframe for this ICD code
      df <- df_base %>%
        mutate(
          ICD_FLAG = diag_wide[[icd]][diag_wide$is_doctor == 1],
          ICD_DATE = ymd(na_if(as.character(diag_wide[[date_col]][diag_wide$is_doctor == 1]), ""))
        )
      
      # Calculate entry and exit dates
      df2 <- df %>%
        mutate(
          ENTRY_DATE = pmax(as.Date("1998-01-01"), BIRTH_DATE, na.rm = TRUE),
          tmp_icd = if_else(ICD_FLAG == 1, ICD_DATE, as.Date("2100-01-01")),
          tmp_death = if_else(DEATH == 1, DEATH_DATE, as.Date("2100-01-01")),
          EXIT_DATE = pmin(tmp_icd, tmp_death, end_of_data, na.rm = TRUE),
          EVENT = as.integer(ICD_FLAG == 1 & !is.na(ICD_DATE) & ICD_DATE <= EXIT_DATE)
        ) %>%
        select(-tmp_icd, -tmp_death)
      
      # Build Lexis object
      L <- Lexis(
        entry = list(
          age = as.numeric(ENTRY_DATE - BIRTH_DATE) / 365.25
        ),
        exit = list(
          age = as.numeric(EXIT_DATE - BIRTH_DATE) / 365.25
        ),
        exit.status = factor(EVENT, levels = c(0, 1), labels = c("no_event", "event")),
        data = df2,
        id = ID
      )
      
      # Split by age bands
      age_breaks <- seq(0, 100, by = 5)
      L_split <- splitLexis(L,
                            breaks = list(age = age_breaks),
                            time.scale = "age")
      
      # Calculate incidence rates by age and sex
      L_tab <- L_split %>%
        mutate(
          age_band = cut(
            age,
            breaks = age_breaks,
            right = FALSE,
            include.lowest = TRUE
          )
        ) %>%
        group_by(SEX, age_band) %>%
        summarise(
          pyrs = sum(lex.dur),
          events = sum(lex.Cst == "no_event" & lex.Xst == "event"),
          .groups = "drop"
        ) %>%
        mutate(
          IR = events / pyrs,
          weight = pyrs / sum(pyrs)
        )
      
      # Calculate adjusted incidence rate
      adj_IR_1k <- sum(L_tab$IR * L_tab$weight) * 1000
      
      # Add to results
      results <- rbind(results, data.frame(
        ICD_CODE = icd,
        adj_IR_1k = adj_IR_1k
      ))
      
    }, error = function(e) {
      warning(paste("Error processing ICD code", icd, ":", e$message))
    })
  }
  
  # Sort by incidence rate (descending)
  results <- results %>%
    arrange(desc(adj_IR_1k))
  
  return(results)
}




















table_wide <- table %>%
    rename(ID = V1, CODE = V2, DATE = V3) %>%
    select(ID, CODE, DATE) %>%
    tidyr::pivot_wider(
        id_cols = ID,
        names_from = CODE,
        values_from = DATE,
        names_glue = "{CODE}_DATE"
    ) %>%
    # add binary indicator (1/0) for each *_DATE column and place it right after the date column
    {
      df <- .
      date_cols <- grep("_DATE$", names(df), value = TRUE)
      if (length(date_cols) > 0) {
        for (dc in date_cols) {
          ind <- sub("_DATE$", "", dc)
          df[[ind]] <- as.integer(!is.na(df[[dc]]))
          df <- dplyr::relocate(df, dplyr::all_of(ind), .after = dplyr::all_of(dc))
        }
      }
      df
    }
gc()

table_wide <- dvv %>% left_join(table_wide, by = c("ID" = "ID"))

























icd_chapter_map <- c(
  "A" = "Certain Infectious and Parasitic Diseases",
  "B" = "Certain Infectious and Parasitic Diseases",
  "C" = "Neoplasms",
  "D" = "Neoplasms / Diseases of the Blood and Blood-forming Organs",
  "E" = "Endocrine, Nutritional and Metabolic Diseases",
  "F" = "Mental and Behavioural Disorders",
  "G" = "Diseases of the Nervous System",
  "H" = "Diseases of the Eye and Adnexa / Diseases of the Ear and Mastoid Process",
  "I" = "Diseases of the Circulatory System",
  "J" = "Diseases of the Respiratory System",
  "K" = "Diseases of the Digestive System",
  "L" = "Diseases of the Skin and Subcutaneous Tissue",
  "M" = "Diseases of the Musculoskeletal System and Connective Tissue",
  "N" = "Diseases of the Genitourinary System",
  "O" = "Pregnancy, Childbirth and the Puerperium",
  "P" = "Certain Conditions Originating in the Perinatal Period",
  "Q" = "Congenital Malformations, Deformations and Chromosomal Abnormalities",
  "R" = "Symptoms, Signs and Abnormal Clinical and Laboratory Findings, Not Elsewhere Classified",
  "S" = "Injury, Poisoning and Certain Other Consequences of External Causes",
  "T" = "Injury, Poisoning and Certain Other Consequences of External Causes",
    "V" = "External Causes of Morbidity and Mortality",
    "W" = "External Causes of Morbidity and Mortality",
    "X" = "External Causes of Morbidity and Mortality",
    "Y" = "External Causes of Morbidity and Mortality",
    "Z" = "Factors Influencing Health Status and Contact with Health Services"
)























# Plot diagnoses: doctors vs non-doctors
p_diag_scatter <- ggplot(dataset_diag, aes(x = adj_IR_1k_nondocs, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
    geom_point(size = POINT_SIZE_NOT_SIG, alpha = ALPHA_NOT_SIG) +
    geom_text_repel(data = top_extreme_diag_docs, aes(label = CODE), 
                        size = 4, 
                        show.legend = FALSE,
                        max.overlaps = Inf,
                        min.segment.length = 0,
                        box.padding = 0.5,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.5) +
    geom_text_repel(data = top_extreme_diag_nondocs, aes(label = CODE), 
                        size = 4, 
                        show.legend = FALSE,
                        max.overlaps = Inf,
                        min.segment.length = 0,
                        box.padding = 0.5,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.5) +
    scale_color_manual(values = cb_palette, name = "Chapter") +
    labs(
        title = "Age and Sex Adjusted Incidence Rates: Doctors vs General Population",
        subtitle = sprintf("Number of diagnoses tested: %d", nrow(dataset_diag)),
        x = "Adjusted IR (per 1,000 person-years) - General Population",
        y = "Adjusted IR (per 1,000 person-years) - Doctors"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        plot.subtitle = element_text(size = TEXT_SIZE_AXIS_TITLE),
        legend.text = element_text(size = TEXT_SIZE_LEGEND),
        legend.title = element_text(size = TEXT_SIZE_LEGEND),
        legend.position = "right"
    )