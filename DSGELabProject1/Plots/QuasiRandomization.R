# ==============================================================================
# Continuity of Care (COC) - Distribution Plots
# ==============================================================================
# This script produces three separate figures:
#   - Suppl. Figure 2:  Histogram of COC values across all patients
#   - Extra:            COC value vs. total number of visits (scatter)
#   - Extra:            COC value density, split by doctor vs. non-doctor patients
# ==============================================================================


## 1. Libraries ------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)

## 2. File paths -------------------------------------------------------------------

InDir  <- "/media/volume/Projects/DSGELabProject1/"
OutDir <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"

if (!dir.exists(OutDir)) dir.create(OutDir, recursive = TRUE)

# -- Inputs --
coc_file        <- paste0(InDir, "patient_COC_info_20250226.csv")
doctor_ids_file <- paste0(InDir, "doctors_20250424.csv")

# -- Outputs --
outfile_supp_fig2_png   <- paste0(OutDir, "Supplementary_Figure2_COC_histogram_20250226.png")
outfile_supp_fig2_pdf   <- paste0(OutDir, "Supplementary_Figure2_COC_histogram_20250226.pdf")

## 3. Shared plotting parameters ---------------------------------------------------

# Bice-Boxerman COC plot parameters 
COC_THRESHOLDS <- c(0.4, 0.7) # based on literature
COC_THRESHOLD_COLOR <- "red"
COC_THRESHOLD_LINETYPE <- "dashed"
COC_BINWIDTH <- 0.05
LABEL_COC <- "Bice-Boxerman Continuity of Care (COC) Index"

# Shared theme for all plots
theme_common <- theme_minimal()

# -- Export dimensions, shared by every PNG/PDF export in this script --
PNG_WIDTH  <- 8
PNG_HEIGHT <- 10
PNG_RES    <- 300
PDF_WIDTH  <- 8.27  # A4 landscape, inches
PDF_HEIGHT <- 10    # A4 landscape, inches

## 4. Load data ---------------------------------------------------------------------

df      <- fread(coc_file)
doc_ids <- fread(doctor_ids_file, header = FALSE)$V1

# Quick sanity check of the COC distribution
summary(df$COC)

## 5. Suppl. Figure 2: histogram of COC values --------------------------------------
# Overall distribution of COC values across all patients, 
# with the reference thresholds marked.

p1 <- ggplot(df, aes(x = COC)) +
geom_histogram(binwidth = COC_BINWIDTH, color = "black", alpha = 0.5) +
geom_vline(xintercept = COC_THRESHOLDS, color = COC_THRESHOLD_COLOR, linetype = COC_THRESHOLD_LINETYPE) +
labs(
    x = LABEL_COC, 
    y = "Count") +
theme_common

## 6. Export --------------------------------------------------------------------------

# Suppl. Figure 2
ggsave(filename = outfile_supp_fig2_png, plot = p1, device = "png", units = "in", width = PNG_WIDTH, height = PNG_HEIGHT, dpi = PNG_RES)
ggsave(filename = outfile_supp_fig2_pdf, plot = p1, device = "pdf", width = PDF_WIDTH, height = PDF_HEIGHT)