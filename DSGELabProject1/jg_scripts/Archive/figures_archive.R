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

