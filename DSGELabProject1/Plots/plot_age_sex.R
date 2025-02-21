library(data.table)
library(ggplot2)
library(patchwork)
library(lubridate)
library(dplyr)

df = fread("/media/volume/Projects/DSGELabProject1/doctor_characteristics_wlongest_Specialty_20250220.csv")

# prepare columns
df$BIRTH_DATE <- ymd(df$BIRTH_DATE)
df$birth_year <- year(df$BIRTH_DATE)
df$SEX <- factor(df$SEX, levels = c(1, 2), labels = c("Male", "Female"))
sex_counts <- df %>%
  group_by(SEX) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Histogram of birth year
p1 <- ggplot(df, aes(x = birth_year)) +
  geom_histogram(binwidth = 1, fill = "black", alpha = 0.6) +
  scale_x_continuous(breaks = seq(min(df$birth_year, na.rm = TRUE), max(df$birth_year, na.rm = TRUE), by = 5)) +
  labs(x = "Birth Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Histogram of birth year by sex ('fill' position)
p2 <- ggplot(df, aes(x = birth_year, fill = SEX)) +
  geom_histogram(position = "fill", binwidth = 1, alpha = 0.6) +
  scale_fill_manual(values = c("blue", "orange")) +
  scale_x_continuous(breaks = seq(min(df$birth_year, na.rm = TRUE), max(df$birth_year, na.rm = TRUE), by = 5)) +
  labs(x = "Birth Year", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Bar plot of sex distribution
p3 <- ggplot(df, aes(x = SEX, fill = SEX)) +
  geom_bar(aes(y = ..count../sum(..count..))) +
  scale_fill_manual(values = c("blue", "orange")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Sex", y = "Frequency") +
  geom_text(data = sex_counts, aes(x = SEX, y = count / sum(sex_counts$count), label = paste0(round(percentage, 1), "%")),vjust = -0.5, size = 3) +
  theme(legend.position = "none")

# Arrange plots
combined_plot <- (p1 / p2) | p3

# Save plot
outpath <- "/media/volume/Projects/DSGELabProject1/Plots/age_sex_plot_20250221.png"
ggsave(filename = outpath, plot = combined_plot, width = 14, height = 8, dpi = 300)