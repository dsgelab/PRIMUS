### ----------------------------------------------------------------------------
### 0. LIBRARIES
### ----------------------------------------------------------------------------

.libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(ggplot2)
library(dplyr)
library(readr)
library(metafor)


### ----------------------------------------------------------------------------
### 1. PATHS
### ----------------------------------------------------------------------------

DATE_DATA <- "20260920"
TODAY     <- format(Sys.Date(), "%Y%m%d")

# --- Input ---
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE_DATA, '/Results_', DATE_DATA, '/Results_ATC_', DATE_DATA, '.csv')

# --- Output ---
OutDir <- paste0("/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/")
if (!dir.exists(OutDir)) dir.create(OutDir, recursive = TRUE)

BASENAME_PLOT <- paste0("Supplements_ChapterAverages_Plot_", TODAY)

### ----------------------------------------------------------------------------
### 2. PARAMETERS 
### ----------------------------------------------------------------------------

# --- Filtering / significance thresholds ---
MIN_CASES   <- 300
PADJ_METHOD <- "bonferroni"
SIG_ALPHA   <- 0.05

# --- Plot styling ---
JITTER_RANGE    <- 0.2
MIN_BOXPLOT_N   <- 3  # minimum number of points required to show a boxplot for a chapter

# --- Plot export settings ---
PLOT_WIDTH  <- 14
PLOT_HEIGHT <- 10
PLOT_DPI    <- 300

# -- Helper: save a ggplot as both PNG and PDF using the same base filename --
save_plot_png_pdf <- function(plot, dir, basename, width, height, dpi = PLOT_DPI) {
    ggsave(filename = file.path(dir, paste0(basename, ".png")), 
      plot = plot,
      width = width, 
      height = height, 
      dpi = dpi
    )
    ggsave(filename = file.path(dir, paste0(basename, ".pdf")), 
      plot = plot,
      width = width, 
      height = height
    )
}

# --- Reference data: ATC chapter names and color palette ---

# Full ATC chapter names keyed by single-letter code
atc_chapter_map <- c(
    "A" = "Alimentary Tract and Metabolism",
    "B" = "Blood and Blood Forming Organs",
    "C" = "Cardiovascular System",
    "D" = "Dermatologicals",
    "G" = "Genito Urinary System and Sex Hormones",
    "H" = "Systemic Hormonal Preparations, \nExcl. Sex Hormones and Insulins",
    "J" = "Antiinfectives for Systemic Use",
    "L" = "Antineoplastic and Immunomodulating Agents",
    "M" = "Musculo-Skeletal System",
    "N" = "Nervous System",
    "P" = "Antiparasitic Products, \nInsecticides and Repellents",
    "R" = "Respiratory System",
    "S" = "Sensory Organs",
    "V" = "Various"
)

# Color-blind friendly palette (one color per chapter)
cb_palette <- c(
    "#E69F00",  # A - Alimentary Tract and Metabolism
    "#56B4E9",  # B - Blood and Blood Forming Organs
    "#009E73",  # C - Cardiovascular System
    "#D55E00",  # D - Dermatologicals
    "#CC79A7",  # G - Genito Urinary System and Sex Hormones
    "#0072B2",  # H - Systemic Hormonal Preparations
    "#F0E442",  # J - Antiinfectives for Systemic Use
    "#999999",  # L - Antineoplastic and Immunomodulating Agents
    "#E7298A",  # M - Musculo-Skeletal System
    "#7570B3",  # N - Nervous System
    "#66A61E",  # P - Antiparasitic Products
    "#A6761D",  # R - Respiratory System
    "#999999",  # S - Sensory Organs
    "#E6AB02"   # V - Various
)

### ----------------------------------------------------------------------------
### 3. LOAD & PREPARE DATA
### ----------------------------------------------------------------------------

dataset <- read_csv(dataset_file, show_col_types = FALSE)
dataset <- dataset[dataset$N_CASES >= MIN_CASES, ]

# Multiple test correction
dataset$PVAL_ADJ           <- p.adjust(dataset$PVAL_ABS_CHANGE, method = PADJ_METHOD)
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ < SIG_ALPHA

# Annotate ATC chapter — preserve original alphabetical-by-letter order
dataset <- dataset %>%
    mutate(
        MED_CHAPTER  = substr(OUTCOME_CODE, 1, 1),
        CHAPTER_NAME = atc_chapter_map[MED_CHAPTER]
    ) %>%
    filter(!is.na(CHAPTER_NAME))

dataset$CHAPTER_NAME <- factor(
    dataset$CHAPTER_NAME,
    levels = unique(atc_chapter_map[sort(unique(dataset$MED_CHAPTER))])
)

chapter_color_map <- setNames(
    cb_palette[seq_len(nlevels(dataset$CHAPTER_NAME))],
    levels(dataset$CHAPTER_NAME)
)

# Reproducible jitter positions for the individual-dot layer of the plot below
set.seed(1)
dataset$x_jittered <- as.numeric(dataset$CHAPTER_NAME) +
    runif(nrow(dataset), -JITTER_RANGE, JITTER_RANGE)


### ----------------------------------------------------------------------------
### 4. PLOT: distribution of absolute change by ATC chapter
### ----------------------------------------------------------------------------

p_supp <- ggplot(dataset, aes(x = CHAPTER_NAME, y = ABS_CHANGE, colour = CHAPTER_NAME, fill = CHAPTER_NAME)) +
    # Background: individual jittered transparent dots
    geom_point(
        aes(x = x_jittered),
        shape = 16,
        size  = 1.5,
        alpha = 0.5
    ) +
    {if (any(dataset %>% count(CHAPTER_NAME) %>% pull(n) >= MIN_BOXPLOT_N)) geom_boxplot(
        aes(x = as.numeric(CHAPTER_NAME)),
        data = dataset %>% group_by(CHAPTER_NAME) %>% filter(n() >= MIN_BOXPLOT_N),
        width         = 0.45,
        alpha         = 0.3,
        outlier.shape = NA,
        linewidth     = 0.6
    )} +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    scale_x_continuous(
        breaks = seq_along(levels(dataset$CHAPTER_NAME)),
        labels = levels(dataset$CHAPTER_NAME)
    ) +
    scale_colour_manual(values = chapter_color_map, guide = "none") +
    scale_fill_manual(values = chapter_color_map, guide = "none") +
    labs(
        x     = NULL,
        y     = "Change in Prescription Rate \n(before vs after event, 3 year window)",
        title = "Distribution of Absolute Change, by ATC Chapter"
    ) +
    theme_minimal() +
    theme(
        axis.text.x        = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y        = element_text(size = 10),
        axis.title.y       = element_text(size = 12),
        plot.title         = element_text(size = 14, face = "bold"),
        panel.grid.major.y = element_line(colour = "grey93"),
        panel.grid.minor   = element_blank(),
        plot.margin        = margin(8, 12, 8, 8)
    )


# Save plot as PNG and PDF
save_plot_png_pdf(p_supp, OutDir, BASENAME_PLOT, PLOT_WIDTH, PLOT_HEIGHT, PLOT_DPI)


### ----------------------------------------------------------------------------
# 5. AVERAGE EFFECT: 
#   (A) summary statistics              (across ALL chapters and WITHIN each chapter)
#   (B) random-effects meta-analysis    (across ALL chapters only)
### ----------------------------------------------------------------------------

MA_METHOD <- "REML"   

BASENAME_AVG_SUMMARY <- paste0("Supplements_AverageEffect_SummaryStats_", TODAY)
BASENAME_AVG_META    <- paste0("Supplements_AverageEffect_MetaAnalysis_", TODAY)

### ---- A. Summary statistics -------------------------------------------------

summarise_effect <- function(df) {
    df %>% summarise(
        N      = n(),
        Mean   = mean(ABS_CHANGE, na.rm = TRUE),
        SD     = sd(ABS_CHANGE, na.rm = TRUE),
        Median = median(ABS_CHANGE, na.rm = TRUE),
        Q1     = quantile(ABS_CHANGE, 0.25, na.rm = TRUE),
        Q3     = quantile(ABS_CHANGE, 0.75, na.rm = TRUE),
    )
}

summary_all <- dataset %>% summarise_effect() %>% mutate(CHAPTER_NAME = "All chapters", .before = 1)
summary_by_chapter <- dataset %>%
    group_by(CHAPTER_NAME) %>%
    summarise_effect() %>%
    mutate(CHAPTER_NAME = as.character(CHAPTER_NAME))

avg_summary_table <- bind_rows(summary_all, summary_by_chapter)
write_csv(avg_summary_table, paste0(OutDir, BASENAME_AVG_SUMMARY, ".csv"))

### ---- B. Random-effects meta-analysis (metafor), all chapters ---------------

# Drop rows that cannot enter a meta-analysis (missing / non-positive SE)
ma_data <- dataset %>% filter(is.finite(ABS_CHANGE), is.finite(ABS_CHANGE_SE), ABS_CHANGE_SE > 0)
if (nrow(ma_data) < nrow(dataset)) {
    warning(nrow(dataset) - nrow(ma_data), " medication(s) dropped from the meta-analysis (missing or zero SE).")
}

fit <- rma(yi = ABS_CHANGE, sei = ABS_CHANGE_SE, data = ma_data, method = MA_METHOD)
pred <- predict(fit)                  
ci   <- confint(fit)$random           

meta_table <- tibble(
    Statistic = c(
        "K (number of medications)",
        "Average effect (mu)",
        "Prediction interval",
        "tau^2 (between-medication variance)",
        "tau (between-medication SD)",
        "I^2 (% of variability due to heterogeneity)",
        "H^2 (total / sampling variance)",
        "Cochran's Q test of heterogeneity"
    ),
    Estimate = c(fit$k, as.numeric(fit$beta), NA, ci["tau^2", "estimate"], ci["tau", "estimate"],
                 ci["I^2(%)", "estimate"], ci["H^2", "estimate"], fit$QE),
    SE       = c(NA, fit$se, NA, fit$se.tau2, NA, NA, NA, NA),
    CI_LB    = c(NA, fit$ci.lb, pred$pi.lb, ci["tau^2", "ci.lb"], ci["tau", "ci.lb"],
                 ci["I^2(%)", "ci.lb"], ci["H^2", "ci.lb"], NA),
    CI_UB    = c(NA, fit$ci.ub, pred$pi.ub, ci["tau^2", "ci.ub"], ci["tau", "ci.ub"],
                 ci["I^2(%)", "ci.ub"], ci["H^2", "ci.ub"], NA),
    P_VALUE  = c(NA, fit$pval, NA, NA, NA, NA, NA, fit$QEp),
)

write_csv(meta_table, paste0(OutDir, BASENAME_AVG_META, ".csv"))