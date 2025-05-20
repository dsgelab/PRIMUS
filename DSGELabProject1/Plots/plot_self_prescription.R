library(data.table)
library(ggplot2)
library(patchwork)
library(dplyr)
library(lubridate)
library(tidyr)

df = fread("/media/volume/Projects/DSGELabProject1/doctor_patient_summary_20250220_v2.csv")
df1 = fread("/media/volume/Projects/DSGELabProject1/doctor_characteristics_wlongest_Specialty_20250220.csv")
df2 = fread("/media/volume/Projects/DSGELabProject1/condensed_specialty_dic.csv")

df2$SPECIALTY = as.character(df2$CODEVALUE)
df1 = df1 %>% rename("DOCTOR_ID" = "FID")
df_plot = merge(df, df1, by = 'DOCTOR_ID')
df_plot = merge(df_plot, df2, by = 'SPECIALTY')

# Calculate percentages and averages by specialty
specialty_summary = df_plot %>%
    group_by(LABEL_EN) %>%
    summarise(
        TotalPrescriptions = sum(Prescriptions),
        TotalSelfPrescriptions = sum(SelfPrescription),
        AvgPrescriptions = mean(Prescriptions)
    ) %>%
    # Order by average prescriptions (descending)
    arrange(AvgPrescriptions) %>%
    mutate(
        LABEL_EN = factor(LABEL_EN, levels = LABEL_EN),
        SelfPercentage = TotalSelfPrescriptions / TotalPrescriptions * 100,
        OtherPercentage = 100 - SelfPercentage
    ) %>%
    pivot_longer(
        cols = c(SelfPercentage, OtherPercentage),
        names_to = "Type",
        values_to = "Percentage"
    )

# Create plot 
p1 = ggplot(specialty_summary, aes(x = Percentage, y = LABEL_EN, fill = Type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
        values = c("SelfPercentage" = "#4682B4", "OtherPercentage" = "#B0C4DE"),
        labels = c("SelfPercentage" = "Self Prescriptions", "OtherPercentage" = "Patient Prescriptions")
    ) +
    labs(x = "Percentage (%)", y = NULL) +
    theme_minimal() +
    theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank()
    )

p2 = ggplot(specialty_summary, aes(x = AvgPrescriptions, y = LABEL_EN)) +
    geom_bar(stat = "identity", fill = "#2E8B57") +
    labs(x = "Average Total Prescriptions", y = NULL) +
    theme_minimal() +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank()
    )

# Combine the plots using patchwork
combined_plot = p1 + p2

# Save  plot
outpath = "/media/volume/Projects/DSGELabProject1/Plots/plot_selfprescriptions_20250407.png"
ggsave(filename = outpath, plot = combined_plot, width = 12, height = 6, dpi = 300)