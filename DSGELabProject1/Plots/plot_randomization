library(data.table)
library(ggplot2)
library(patchwork)
library(dplyr)

df = fread("/media/volume/Projects/DSGELabProject1/doctor_patient_summary_20250220.csv")
df1 = fread("/media/volume/Projects/DSGELabProject1/doctor_characteristics_wlongest_Specialty_20250220.csv")

# prepare columns
df$Freq_Private = df$PrivatePrescriptions / (df$PrivatePrescriptions + df$PublicPrescriptions) * 100
df_clean <- df %>% filter(!is.na(Freq_Private))

# Violin plot for Private sector prescription frequency
p1 <- ggplot(df_clean, aes(x = factor(1), y = Freq_Private)) +
  geom_violin(fill = "black", alpha = 0.6) +
  labs(x = "", y = "Frequency of private sector prescriptions (%)") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Scatter plot of patient randomization
lm_model <- lm(TotalPatients ~ UniquePatients, data = df)
regression_coef <- coef(lm_model)[2] 
p2 <- ggplot(df, aes(x = UniquePatients, y = TotalPatients, color = Freq_Private)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Regression line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +  # Dashed line (β = 1)
  labs(x = "N of Unique Patients", y = "N of Total Patients (i.e. visits)", color = "Private (%)") +
  scale_color_viridis_c(option = "viridis") +
  annotate("text", x = max(df$UniquePatients, na.rm = TRUE) * 0.8, 
           y = max(df$TotalPatients, na.rm = TRUE) * 0.9, 
           label = paste("β: ", round(regression_coef, 2)), 
           color = "black", size = 5) +
  theme_minimal()

# Combine plots
combined_plot <- p1 | p2

# Save  plot
outpath <- "/media/volume/Projects/DSGELabProject1/Plots/plot_randomization_bysector_20250221.png"  
ggsave(filename = outpath, plot = combined_plot, width = 12, height = 6, dpi = 300)

# --------------------------------------------
# prepare columns
df1 = df1 %>% rename("DOCTOR_ID" = "FID")
df_plot = merge(df, df1, by = 'DOCTOR_ID')

# prepare columns (order and color by specialty)
specialty_freq <- df_plot %>%
  count(SPECIALTY) %>%
  arrange(desc(n)) 
df_plot <- df_plot %>%
  left_join(specialty_freq, by = "SPECIALTY") %>%
  mutate(SPECIALTY = factor(SPECIALTY, levels = specialty_freq$SPECIALTY))

color_palette <- viridis::viridis(n = length(unique(df_plot$SPECIALTY)), option = "D")

# Frequency Bar Plot
p1 <- ggplot(df_plot, aes(x = SPECIALTY)) + 
  geom_bar(aes(fill = SPECIALTY), alpha = 0.8) +  
  coord_flip() +  
  scale_fill_manual(values = color_palette, guide = "none") +
  labs(x = "Specialty", y = "Frequency") +
  theme_minimal()

# Scatter Plot colored by Specialty with Regression Lines
p2 <- ggplot(df_plot, aes(x = UniquePatients, y = TotalPatients, color = SPECIALTY)) +  
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +  
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +  
  labs(x = "N of Unique Patients", y = "N of Total Patients (i.e. visits)", color = "Specialty") +
  scale_color_manual(values = color_palette, guide=FALSE) +
  ylim(0, max(df_plot$TotalPatients)) +  
  theme_minimal()

# Combine the plots
combined_plot <- p1 | p2

# Save  plot
outpath <- "/media/volume/Projects/DSGELabProject1/Plots/plot_randomization_byspecialty_20250221.png"  
ggsave(filename = outpath, plot = combined_plot, width = 12, height = 6, dpi = 300)