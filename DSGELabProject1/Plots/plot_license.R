library(data.table)
library(ggplot2)
library(patchwork)

df = fread("/media/volume/Projects/DSGELabProject1/doctor_characteristics_wlongest_Specialty_20250220.csv")

# prepare date columns
df$start_date <- as.Date(df$start_date, format = "%y-%m-%d")
df$end_date <- as.Date(df$end_date, format = "%y-%m-%d")
df$start_year <- year(df$start_date)
df$end_year <- year(df$end_date)
df$license_days <- as.numeric(difftime(df$end_date, df$start_date, units = "days"))

# Create histograms of start and end date (by year)
p1 <- ggplot(df, aes(x = start_year)) +
  geom_histogram(binwidth = 1, fill = "black", alpha = 0.6) +
  scale_x_continuous(breaks = seq(min(df$start_year), max(df$start_year), by = 2)) +
  labs(x = "License Start Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p2 <- ggplot(df, aes(x = end_year)) +
  geom_histogram(binwidth = 1, fill = "black", alpha = 0.6) +
  scale_x_continuous(breaks = seq(min(df$end_year), max(df$end_year), by = 1)) +
  labs(x = "License End Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Scatter plot with regression line and correlation
cor_coeff <- cor(df$practicing_days, df$license_days)
p3 <- ggplot(df, aes(x = practicing_days, y = license_days)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Practicing Days", y = "License Days (end - start)") +
  annotate("text", x = Inf, y = Inf, label = paste("r =", round(cor_coeff, 2)),
           hjust = 1.1, vjust = 1.1, size = 4)

# Arrange plots
combined_plot <- (p1 / p2) | p3

# Save plot
outpath <- "/media/volume/Projects/DSGELabProject1/Plots/license_plot_20250221.png"
ggsave(filename = outpath, plot = combined_plot, width = 14, height = 8, dpi = 300)