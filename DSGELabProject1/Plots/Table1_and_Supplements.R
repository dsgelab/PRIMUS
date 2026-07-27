# ==============================================================================
# Table 1 and Supplementary Materials
# ==============================================================================
# This script produces:
#   - Table 1:            Cohort characteristics (general)
#   - Suppl. Figure 1:    Cohort characteristics (specialty) - 3 panels
#   - Suppl. Table 1:     Cohort characteristics (specialty)
#   - Extra:              Total prescriptions vs. N doctors, by specialty
# ==============================================================================


## 1. Libraries ----------------------------------------------------------------

.libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(data.table)
library(table1)
library(arrow)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(ggrepel)   


## 2. File paths ----------------------------------------------------------------

TODAY   <- format(Sys.Date(), "%Y%m%d")
InDir  <- "/media/volume/Projects/DSGELabProject1/"
OutDir <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"

if (!dir.exists(OutDir)) dir.create(OutDir, recursive = TRUE)

# -- Inputs --
doctor_cohort_file          <- paste0(InDir, "doctors_20250424.csv")
doctor_characteristics_file <- paste0(InDir, "doctor_characteristics_20250520.csv")
doctor_prescription_file    <- paste0(InDir, "doctor_imputed_prescription_summary_20260618.csv")

# -- Outputs: Table 1 --
outfile_table1_csv  <- paste0(OutDir, "Table1_", TODAY, ".csv")

# -- Outputs: Suppl. Figure 1 --
outfile_supp_fig1_png <- paste0(OutDir, "Supplementary_Figure1_", TODAY, ".png")
outfile_supp_fig1_pdf <- paste0(OutDir, "Supplementary_Figure1_", TODAY, ".pdf")  

# -- Outputs: Extra Figure --
outfile_extra_fig_png <- paste0(OutDir, "Extra_Figure_", TODAY, ".png")
outfile_extra_fig_pdf <- paste0(OutDir, "Extra_Figure_", TODAY, ".pdf")  

# -- Outputs: Suppl. Table 1 --
outfile_supp_table1_csv <- paste0(OutDir, "Supplementary_Table1_", TODAY, ".csv")


## 3. Shared plotting parameters -------------------------------------------------

# -- Colour palette --
C_MALE      <- "#0173B2"   # Panel A (sex density)
C_FEMALE    <- "#DE8F05"   # Panel A (sex density)
C_SPECIALTY <- "#117733"   # Panels B & C, and Extra Figure (specialty-based plots)

# -- Font sizes (pt) --
FONT_AXIS_TITLE      <- 9  # axis titles, all panels
FONT_AXIS_TEXT       <- 7  # axis tick labels, all panels
FONT_AXIS_TEXT_SMALL <- 6  # axis tick labels where space is tight (long specialty names)
FONT_LEGEND          <- 7  # legend title/text, Panel A
FONT_PANEL_LABEL     <- 10 # bold "A."/"B."/"C." panel tags

# -- Common theme layer --
# Added on top of theme_minimal() in each panel below; panel-specific theme()
# calls after this one can still override individual elements as needed.
theme_common <- theme(
  axis.title.x = element_text(size = FONT_AXIS_TITLE),
  axis.title.y = element_text(size = FONT_AXIS_TITLE),
  axis.text.x  = element_text(size = FONT_AXIS_TEXT),
  axis.text.y  = element_text(size = FONT_AXIS_TEXT),
  plot.margin  = margin(5, 5, 5, 5)
)

# -- Export dimensions, shared by every PNG/PDF export in this script --
PNG_WIDTH  <- 2400
PNG_HEIGHT <- 1800
PNG_RES    <- 300
PDF_WIDTH  <- 11.69  # A4 landscape, inches
PDF_HEIGHT <- 8.27   # A4 landscape, inches


## 4. Main ----------------------------------------------------------------------------

data        <- fread(doctor_characteristics_file)
doctor_list <- fread(doctor_cohort_file, header = FALSE)$V1

# QC: keep only doctors in cohort
data <- data[DOCTOR_ID %in% doctor_list]
data[, BIRTH_DATE := as.Date(BIRTH_DATE)]
data[, DEATH_DATE := as.Date(DEATH_DATE)]

# 4.1 N doctors
n_doctors <- length(unique(data$DOCTOR_ID))

# 4.2 Sex (1 = Male, 2 = Female)
data[, SEX_LABEL := factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))]

# 4.3 Age in 1998
data[, BIRTH_YEAR  := as.integer(format(BIRTH_DATE, "%Y"))]
data[, AGE_IN_1998 := 1998L - BIRTH_YEAR]

# 4.4 Follow-up
# Follow-up window per doctor is bounded by: 
# study period (1998-2022), license validity (START_DATE/END_DATE), age 60 cutoff, and death.
data[, LICENSE_START   := as.Date(START_DATE)]
data[, LICENSE_END     := as.Date(END_DATE)]
data[, FOLLOW_UP_START := pmax(as.Date("1998-01-01"), LICENSE_START, na.rm = TRUE)]
data[, FOLLOW_UP_END   := pmin(as.Date("2022-12-31"), LICENSE_END, BIRTH_DATE + 60 * 365.25, DEATH_DATE, na.rm = TRUE)]
data[, FOLLOW_UP       := pmax(as.numeric(FOLLOW_UP_END - FOLLOW_UP_START) / 365.25, 0)] # put 0 if negative
data[, FOLLOW_UP_START_YEAR := as.integer(format(FOLLOW_UP_START, "%Y"))]
data[, FOLLOW_UP_END_YEAR   := as.integer(format(FOLLOW_UP_END, "%Y"))]

# Print person-years of follow-up
total_person_years <- sum(data$FOLLOW_UP, na.rm = TRUE)
cat(sprintf("Total person-years of follow-up: %s\n", round(total_person_years, 1)))
cat(sprintf("Total person-years of follow-up (thousands): %s\n", round(total_person_years / 1e3, 2)))
cat(sprintf("Total person-years of follow-up (millions): %s\n", round(total_person_years / 1e6, 2)))

# 4.5 Overview of prescriptions
# Prescriptions are only counted while a doctor is within their own follow-up window (see 4.4)
prescription_info <- fread(doctor_prescription_file)
prescription_info <- prescription_info[, c("DOCTOR_ID", "YEAR", "PRESCRIPTION_OUT")]
prescription_info <- prescription_info[DOCTOR_ID %in% doctor_list]
prescription_info <- merge(
  prescription_info,
  data[, .(DOCTOR_ID, FOLLOW_UP_START_YEAR, FOLLOW_UP_END_YEAR)],
  by = "DOCTOR_ID",
  all.x = TRUE
)
prescription_info <- prescription_info[YEAR >= FOLLOW_UP_START_YEAR & YEAR <= FOLLOW_UP_END_YEAR]

# Print total prescriptions
total_prescriptions <- sum(prescription_info$PRESCRIPTION_OUT, na.rm = TRUE)
cat(sprintf("Total prescriptions, during follow-up: %s\n", total_prescriptions))

# 4.5.A Yearly prescriptions (average per doctor, across their follow-up years)
avg_prescriptions <- prescription_info[
  , .(AVG_PRESCRIPTIONS = mean(PRESCRIPTION_OUT, na.rm = TRUE)),
  by = DOCTOR_ID
]

data <- merge(data, avg_prescriptions, by = "DOCTOR_ID", all.x = TRUE)
data[is.na(AVG_PRESCRIPTIONS), AVG_PRESCRIPTIONS := 0]

# 4.5.B Self-prescriptions (going-out): 
# prescriptions a doctor writes for themself, as a share of all prescriptions the doctor writes.
self_prescriptions_out <- fread(doctor_prescription_file)
self_prescriptions_out <- self_prescriptions_out[, .(DOCTOR_ID, YEAR, PRESCRIPTION_OUT, SELF_PRESCRIPTION)]
self_prescriptions_out <- self_prescriptions_out[DOCTOR_ID %in% doctor_list]
self_prescriptions_out <- merge(
  self_prescriptions_out,
  data[, .(DOCTOR_ID, FOLLOW_UP_START_YEAR, FOLLOW_UP_END_YEAR)],
  by = "DOCTOR_ID",
  all.x = TRUE
)
self_prescriptions_out <- self_prescriptions_out[YEAR >= FOLLOW_UP_START_YEAR & YEAR <= FOLLOW_UP_END_YEAR]
self_prescriptions_out <- self_prescriptions_out[
  , .(
    Prescriptions    = sum(PRESCRIPTION_OUT, na.rm = TRUE),
    SelfPrescription = sum(SELF_PRESCRIPTION, na.rm = TRUE)
  ),
  by = DOCTOR_ID
]

self_prescriptions_out[, SELF_PRESCRIPTION_RATE_OUT := fifelse(Prescriptions == 0, NA_real_, 100 * SelfPrescription / Prescriptions)]
avg_self_prescription_rate <- mean(self_prescriptions_out$SELF_PRESCRIPTION_RATE_OUT, na.rm = TRUE)
summary(self_prescriptions_out$SELF_PRESCRIPTION_RATE_OUT)

data <- merge(
  data,
  self_prescriptions_out[, .(
    DOCTOR_ID,
    SELF_PRESCRIPTIONS = SelfPrescription,
    TOTAL_PRESCRIPTIONS_OUT = Prescriptions,
    SELF_PRESCRIPTION_RATE_OUT
  )],
  by = "DOCTOR_ID",
  all.x = TRUE
)
data[is.na(SELF_PRESCRIPTIONS), SELF_PRESCRIPTIONS := 0]
data[is.na(TOTAL_PRESCRIPTIONS_OUT), TOTAL_PRESCRIPTIONS_OUT := 0]
data[is.na(SELF_PRESCRIPTION_RATE_OUT), SELF_PRESCRIPTION_RATE_OUT := NA_real_]

# 4.5.C Self-prescriptions (going-in): 
# prescriptions a doctor writes for themself, as a share of all prescriptions the doctor receives.
self_prescriptions_in <- fread(doctor_prescription_file)
self_prescriptions_in <- self_prescriptions_in[, .(DOCTOR_ID, YEAR, PRESCRIPTION_IN, SELF_PRESCRIPTION)]
self_prescriptions_in <- self_prescriptions_in[DOCTOR_ID %in% doctor_list]
self_prescriptions_in <- merge(
  self_prescriptions_in,
  data[, .(DOCTOR_ID, FOLLOW_UP_START_YEAR, FOLLOW_UP_END_YEAR)],
  by = "DOCTOR_ID",
  all.x = TRUE
)
self_prescriptions_in <- self_prescriptions_in[YEAR >= FOLLOW_UP_START_YEAR & YEAR <= FOLLOW_UP_END_YEAR]
self_prescriptions_in <- self_prescriptions_in[
  , .(
    Prescriptions    = sum(PRESCRIPTION_IN, na.rm = TRUE),
    SelfPrescription = sum(SELF_PRESCRIPTION, na.rm = TRUE)
  ),
  by = DOCTOR_ID
]

self_prescriptions_in[, SELF_PRESCRIPTION_RATE_IN := fifelse(Prescriptions == 0, NA_real_, 100 * SelfPrescription / Prescriptions)]
avg_self_prescription_rate_in <- mean(self_prescriptions_in$SELF_PRESCRIPTION_RATE_IN, na.rm = TRUE)
summary(self_prescriptions_in$SELF_PRESCRIPTION_RATE_IN)

data <- merge(
  data,
  self_prescriptions_in[, .(
    DOCTOR_ID,
    TOTAL_PRESCRIPTIONS_IN = Prescriptions,
    SELF_PRESCRIPTION_RATE_IN
  )],
  by = "DOCTOR_ID",
  all.x = TRUE
)
data[is.na(TOTAL_PRESCRIPTIONS_IN), TOTAL_PRESCRIPTIONS_IN := 0]
data[is.na(SELF_PRESCRIPTION_RATE_IN), SELF_PRESCRIPTION_RATE_IN := NA_real_]

# Derived specialty label used throughout the supplementary outputs below
data[, SPECIALTY := fifelse(INTERPRETATION == "", "No specialty", INTERPRETATION)]

## 5. Table 1 setup ---------------------------------------------------------------
# Assigns human-readable labels, defines custom renderers for categorical vs. continuous variables, and builds the table1 object.

label(data$SEX_LABEL)                  <- "Sex"
label(data$AGE_IN_1998)                <- "Age in 1998 (years)"
label(data$FOLLOW_UP)                  <- "Follow-up (years)"
label(data$AVG_PRESCRIPTIONS)          <- "Yearly prescriptions"
label(data$SELF_PRESCRIPTION_RATE_OUT) <- "Self-prescription rate \n(written to self / all written)"
label(data$SELF_PRESCRIPTION_RATE_IN)  <- "Self-prescription rate \n(written to self / all received)"

# All categorical: N (%)
render_cat <- function(x, ...) {
  c("", sapply(stats.apply.rounding(stats.default(x), ...), function(y)
    sprintf("%s (%.1f)", format(y[["FREQ"]], big.mark = ","), as.numeric(y[["PCT"]]))))
}

# All continuous: Mean (SD) on first line, Median (Min, Max) on second.
# Self-prescription rate variables are percentages, so append a "%" suffix.
render_cont <- function(x, ...) {
  lab <- attr(x, "label")
  is_pct <- !is.null(lab) && grepl("Self-prescription", lab, ignore.case = TRUE)
  pct_suffix <- if (is_pct) "%%" else ""

  c("",
    "Mean (SD)"         = sprintf(paste0("%.2f", pct_suffix, " (%.2f", pct_suffix, ")"),
                            mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
    "Median (Min, Max)" = sprintf(paste0("%.2f", pct_suffix, " (%.2f", pct_suffix, ", %.2f", pct_suffix, ")"),
                            median(x, na.rm = TRUE),
                            min(x, na.rm = TRUE),
                            max(x, na.rm = TRUE)))
}

# Build table
tbl <- table1(
  ~ SEX_LABEL + AGE_IN_1998 + FOLLOW_UP + AVG_PRESCRIPTIONS + SELF_PRESCRIPTION_RATE_OUT + SELF_PRESCRIPTION_RATE_IN,
  data               = data,
  caption            = "Table 1. Characteristics of doctors.",
  render.continuous  = render_cont,
  render.categorical = render_cat,
  rowlabelhead       = "Characteristic"
)


## 6. Export: Table 1 --------------------------------------------------------------

# CSV
tbl_csv <- as.data.frame(tbl)
names(tbl_csv)[1] <- "Characteristic"
write.csv(tbl_csv, file = outfile_table1_csv, row.names = FALSE)

## 7. Suppl. Figure 1: cohort characteristics by specialty ------------------------
# Panel A. Density of sex across birth years
# Panel B. Barplot of specialty frequencies
# Panel C. Violin plot of yearly prescriptions, by specialty

# Panel A
pA <- ggplot(
  data,
  aes(x = BIRTH_YEAR, color = SEX_LABEL, fill = SEX_LABEL)
) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  scale_color_manual(values = c("Male" = C_MALE, "Female" = C_FEMALE)) +
  scale_fill_manual(values = c("Male" = C_MALE, "Female" = C_FEMALE)) +
  labs(
    x = "Birth year",
    y = "Density",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme_common +
  theme(
    legend.position = "top",
    legend.key.size = grid::unit(0.28, "cm"),
    legend.spacing.x = grid::unit(0.08, "cm"),
    legend.spacing.y = grid::unit(0.02, "cm"),
    legend.title = element_text(size = FONT_LEGEND),
    legend.text  = element_text(size = FONT_LEGEND)
  )

# Panel B
specialty_freqs <- data[, .N, by = SPECIALTY][order(-N)]
pB <- ggplot(specialty_freqs, aes(x = reorder(SPECIALTY, N), y = N)) +
  geom_col(fill = C_SPECIALTY) +
  geom_text(aes(label = N), hjust = -0.1, size = 2) +
  coord_flip() +
  labs(
    x = "Specialty",
    y = "Number of doctors"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  theme_minimal() +
  theme_common +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = FONT_AXIS_TEXT_SMALL)
  )

# Panel C
pC <- ggplot(data, aes(x = factor(SPECIALTY, levels = specialty_freqs$SPECIALTY), y = AVG_PRESCRIPTIONS)) +
  geom_violin(fill = C_SPECIALTY, color = "gray30", alpha = 0.6, scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.08, outlier.shape = NA, color = "gray20", fill = "white", linewidth = 0.4) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Specialty",
    y = "Average yearly prescriptions"
  ) +
  theme_minimal() +
  theme_common +
  theme(
    axis.text.x  = element_text(angle = 30, hjust = 1, vjust = 1, size = FONT_AXIS_TEXT_SMALL),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Create labeled grobs so panel labels ("A.", "B.", "C.") sit outside each panel
pA_g <- arrangeGrob(pA, top = grid::textGrob("A.", x = 0, hjust = 0, gp = grid::gpar(fontsize = FONT_PANEL_LABEL, fontface = "bold")))
pB_g <- arrangeGrob(pB, top = grid::textGrob("B.", x = 0, hjust = 0, gp = grid::gpar(fontsize = FONT_PANEL_LABEL, fontface = "bold")))
pC_g <- arrangeGrob(pC, top = grid::textGrob("C.", x = 0, hjust = 0, gp = grid::gpar(fontsize = FONT_PANEL_LABEL, fontface = "bold")))

# Arrange panels in 2 rows: 
# A and B side-by-side in first row, C full-width in second
layout_mat <- rbind(
  c(1, 2),
  c(3, 3)
)
combined <- arrangeGrob(
  pA_g, pB_g, pC_g,
  layout_matrix = layout_mat,
  widths  = c(1, 1),
  heights = c(1, 1)
)

# Export PNG
png(outfile_supp_fig1_png, width = PNG_WIDTH, height = PNG_HEIGHT, res = PNG_RES)
grid::grid.draw(combined)
dev.off()

# Export PDF
pdf(outfile_supp_fig1_pdf, width = PDF_WIDTH, height = PDF_HEIGHT)
grid::grid.draw(combined)
dev.off()

## 8. Extra: total prescriptions vs. N doctors, by specialty ------------

fig2_df <- data[, .(
  N_doctors       = .N,
  Total_Presc_Out = sum(TOTAL_PRESCRIPTIONS_OUT, na.rm = TRUE)
), by = SPECIALTY]

p_fig2 <- ggplot(fig2_df, aes(x = N_doctors, y = Total_Presc_Out)) +
  geom_point(color = "black", size = 2.5, alpha = 0.85) +
  geom_text_repel(aes(label = SPECIALTY), color = "black", size = 3, max.overlaps = Inf) +
  scale_y_log10(labels = scales::comma, expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_log10(labels = scales::comma, expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    x = "Number of doctors",
    y = "Total prescriptions",
    color = "Specialty"
  ) +
  theme_minimal() +
  theme_common +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )

if (!dir.exists(OutDir)) dir.create(OutDir, recursive = TRUE)

# Export PNG
png(outfile_extra_fig_png, width = PNG_WIDTH, height = PNG_HEIGHT, res = PNG_RES)
print(p_fig2)
dev.off()

# Export PDF
pdf(outfile_extra_fig_pdf, width = PDF_WIDTH, height = PDF_HEIGHT)
print(p_fig2)
dev.off()

## 9. Suppl. Table 1: cohort characteristics by specialty -------------------------
# Same metrics as Table 1, broken out by specialty, plus a "Total" row.

supp_table <- data[, .(
  N                                       = .N,
  Pct_Female                              = sprintf("%.2f%%", 100 * mean(SEX == 2, na.rm = TRUE)),
  Age_1998_Mean_SD                        = sprintf("%.2f (%.2f)", mean(AGE_IN_1998, na.rm = TRUE), sd(AGE_IN_1998, na.rm = TRUE)),
  FollowUp_Mean_SD                        = sprintf("%.2f (%.2f)", mean(FOLLOW_UP, na.rm = TRUE), sd(FOLLOW_UP, na.rm = TRUE)),
  Avg_Presc_Mean_SD                       = sprintf("%.2f (%.2f)", mean(AVG_PRESCRIPTIONS, na.rm = TRUE), sd(AVG_PRESCRIPTIONS, na.rm = TRUE)),
  Total_Presc_Out                         = sum(TOTAL_PRESCRIPTIONS_OUT, na.rm = TRUE),
  Total_Self_Presc_Out                    = sum(SELF_PRESCRIPTIONS, na.rm = TRUE),
  Self_Presc_Out_Rate_over_Total_Outgoing = sprintf("%.2f%%", 100 * sum(SELF_PRESCRIPTIONS, na.rm = TRUE) / sum(TOTAL_PRESCRIPTIONS_OUT, na.rm = TRUE)),
  Self_Presc_In_Rate_over_Total_Incoming  = sprintf("%.2f%%", 100 * sum(SELF_PRESCRIPTIONS, na.rm = TRUE) / sum(TOTAL_PRESCRIPTIONS_IN, na.rm = TRUE))
), keyby = SPECIALTY]

supp_table_total <- data[, .(
  SPECIALTY                                = "Total",
  N                                        = .N,
  Pct_Female                               = sprintf("%.2f%%", 100 * mean(SEX == 2, na.rm = TRUE)),
  Age_1998_Mean_SD                         = sprintf("%.2f (%.2f)", mean(AGE_IN_1998, na.rm = TRUE), sd(AGE_IN_1998, na.rm = TRUE)),
  FollowUp_Mean_SD                         = sprintf("%.2f (%.2f)", mean(FOLLOW_UP, na.rm = TRUE), sd(FOLLOW_UP, na.rm = TRUE)),
  Avg_Presc_Mean_SD                        = sprintf("%.2f (%.2f)", mean(AVG_PRESCRIPTIONS, na.rm = TRUE), sd(AVG_PRESCRIPTIONS, na.rm = TRUE)),
  Total_Presc_Out                          = sum(TOTAL_PRESCRIPTIONS_OUT, na.rm = TRUE),
  Total_Self_Presc_Out                     = sum(SELF_PRESCRIPTIONS, na.rm = TRUE),
  Self_Presc_Out_Rate_over_Total_Outgoing  = sprintf("%.2f%%", 100 * sum(SELF_PRESCRIPTIONS, na.rm = TRUE) / sum(TOTAL_PRESCRIPTIONS_OUT, na.rm = TRUE)),
  Self_Presc_In_Rate_over_Total_Incoming   = sprintf("%.2f%%", 100 * sum(SELF_PRESCRIPTIONS, na.rm = TRUE) / sum(TOTAL_PRESCRIPTIONS_IN, na.rm = TRUE))
)]

supp_table <- rbind(supp_table, supp_table_total, fill = TRUE)
setnames(supp_table, c(
  "SPECIALTY",
  "N",
  "Female (%)",
  "Mean Age in 1998 (SD)",
  "Mean Follow-up (SD)",
  "Mean Yearly Prescriptions (SD)",
  "Total Prescriptions",
  "Total Self-Prescriptions",
  "Self-Prescription Rate (written to self / all written)",
  "Self-Prescription Rate (written to self / all received)"
))

# CSV (rows = specialties, cols = metrics)
write.csv(
  supp_table,
  file      = outfile_supp_table1_csv,
  row.names = FALSE
)