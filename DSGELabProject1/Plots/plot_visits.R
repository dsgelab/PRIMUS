library(data.table)
library(ggplot2)
library(patchwork)
library(dplyr)
library(lubridate)

df = fread("/media/volume/Projects/DSGELabProject1/doctor_patient_summary_20250220.csv")
df1 = fread("/media/volume/Projects/DSGELabProject1/doctor_characteristics_wlongest_Specialty_20250220.csv")
df1 = df1 %>% rename("DOCTOR_ID" = "FID")
df_plot = merge(df, df1, by = 'DOCTOR_ID')

# prepare columns
df_plot$Diagnosis <- df_plot$DiagnosisAvohilmo + df_plot$DiagnosisHilmo
df_plot$BIRTH_DATE <- ymd(df_plot$BIRTH_DATE)
df_plot$birth_year <- year(df_plot$BIRTH_DATE)
df_plot$SEX <- factor(df_plot$SEX, levels = c(1, 2), labels = c("Male", "Female"))

# Function to extract regression coefficients
get_coeff <- function(x, y) {
  model <- lm(y ~ x)
  coef <- round(coef(model), 2)
  paste0("β: ", coef[2])
}

# Scatter plot for Prescriptions vs Purchases 
p1A <- ggplot(df_plot, aes(x = Prescriptions, y = Purchases)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Prescriptions", y = "Purchases") +
  annotate("text", x = Inf, y = Inf, label = get_coeff(df_plot$Prescriptions, df_plot$Purchases), 
           hjust = 1.1, vjust = 2, size = 4, color = "blue") +
  theme_minimal()

# Color by year of birth (proxy for adjusting by register bias)
p1B <- ggplot(df_plot, aes(x = Prescriptions, y = Purchases, color = birth_year)) +
  geom_point() +
  labs(x = "Prescriptions", y = "Purchases") +
  theme_minimal() +
  scale_color_viridis_c()

# Color by sex
p1C <- ggplot(df_plot, aes(x = Prescriptions, y = Purchases, color = SEX)) +
  geom_point() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "orange")) +
  labs(x = "Prescriptions", y = "Purchases") +
  theme_minimal()

# Scatter plot for Prescriptions vs Diagnosis 
p2A <- ggplot(df_plot, aes(x = Prescriptions, y = Diagnosis)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Prescriptions", y = "Diagnosis") +
  annotate("text", x = Inf, y = Inf, label = get_coeff(df_plot$Prescriptions, df_plot$Diagnosis), 
           hjust = 1.1, vjust = 2, size = 4, color = "blue") +
  theme_minimal()

# Color by year of birth (proxy for adjusting by register bias)
p2B <- ggplot(df_plot, aes(x = Prescriptions, y = Diagnosis, color = birth_year)) +
  geom_point() +
  labs(x = "Prescriptions", y = "Diagnosis") +
  theme_minimal() +
  scale_color_viridis_c()

# Color by sex
p2C <- ggplot(df_plot, aes(x = Prescriptions, y = Diagnosis, color = SEX)) +
  geom_point() +
  scale_color_manual(values = c("Male" = "blue", "Female" = "orange")) +
  labs(x = "Prescriptions", y = "Diagnosis") +
  theme_minimal()

# Combine plots
combined_plot <- (p1A / p1B / p1C) | (p2A / p2B / p2C) 

# Save  plot
outpath <- "/media/volume/Projects/DSGELabProject1/Plots/plot_visit_description_20250221.png"  
ggsave(filename = outpath, plot = combined_plot, width = 12, height = 6, dpi = 300)

# EXTRA ANALYSIS:
# evaluate if sex is associated with number of diagnsoses and prescriptions 

# T-test for Purchases by SEX
ttest_pres <- t.test(Purchases ~ SEX, data = df_plot)
cat("\nT-test for Purchases ~ SEX:\n")
print(ttest_pres)

# T-test for Prescriptions by SEX
ttest_pres <- t.test(Prescriptions ~ SEX, data = df_plot)
cat("\nT-test for Prescriptions ~ SEX:\n")
print(ttest_pres)

# T-test for Diagnosis by SEX
ttest_diag <- t.test(Diagnosis ~ SEX, data = df_plot)
cat("T-test for Diagnosis ~ SEX:\n")
print(ttest_diag)