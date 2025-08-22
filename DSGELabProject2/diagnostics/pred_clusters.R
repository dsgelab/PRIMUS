library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
setwd("/media/volume/Projects/mikael/XGBoost/results")
source("/media/volume/Projects/mikael/utils.R")

pred <- fread("xgb_extensive_predictions_2025-08-14-1958.csv") %>% as_tibble()
pos_rate <- round(mean(pred$PRESCRIBED), 3)

# PRED_CLASS_LABELS <- c(
#   "Prescribed, 0 <= pred <= 0.2",
#   "Prescribed, 0.2 < pred <= 0.8",
#   "Prescribed, 0.8 < pred <= 1",
#   paste0("Not Prescribed, pred <= ", pos_rate),
#   paste0("Not Prescribed, pred > ", pos_rate)
# )
PRED_CLASS_LABELS <- c(
  "0 <= pred <= 0.2",
  "0.6 <= pred <= 1"
)

# pred <- pred %>%
#   mutate(
#     PRED_CLASS = factor(case_when(
#       PRESCRIBED == 1 & XGB_PRED <= 0.2 ~ PRED_CLASS_LABELS[1],
#       PRESCRIBED == 1 & XGB_PRED > 0.2 & XGB_PRED <= 0.8 ~ PRED_CLASS_LABELS[2],
#       PRESCRIBED == 1 & XGB_PRED > 0.8 & XGB_PRED <= 1 ~ PRED_CLASS_LABELS[3],
#       PRESCRIBED == 0 & XGB_PRED <= pos_rate ~ PRED_CLASS_LABELS[4],
#       PRESCRIBED == 0 & XGB_PRED > pos_rate ~ PRED_CLASS_LABELS[5],
#       TRUE ~ NA_character_
#     ), levels = PRED_CLASS_LABELS)
#   )
pred <- pred %>%
  mutate(
    PRED_CLASS = factor(case_when(
      XGB_PRED <= 0.2 ~ PRED_CLASS_LABELS[1],
      XGB_PRED >= 0.6 ~ PRED_CLASS_LABELS[2],
      TRUE ~ NA_character_
    ), levels = PRED_CLASS_LABELS)
  )

plot_theme <- theme(
    plot.title = element_text(size = 28),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 22),
    legend.text = element_text(size = 20),
    strip.text = element_text(size = 18)
)

ggplot(pred, aes(x = XGB_PRED, fill = factor(PRESCRIBED))) +
    geom_density(
        alpha = 0.4
    ) +
    scale_fill_manual(
        values = c("0" = "#f4c0bd", "1" = "#91dddf"),
        labels = c("0" = "Did not prescribe", "1" = "Prescribed")
    ) +
    labs(
        title = "Distribution of Predicted Probabilities by True Label",
        x = "Predicted probability",
        y = "Probability density",
        fill = "Prescribed"
    ) +
    ylim(0, 50) +
    plot_theme

feature_hist_by_pred_class <- function (feature, label) {
  is_binary <- all(na.omit(pred[[feature]]) %in% c(0, 1))
  is_categorical <- is.factor(pred[[feature]]) || is.character(pred[[feature]]) || is_binary
  pred_all <- pred %>% mutate(PRED_CLASS = "All")
  pred_combined <- bind_rows(
    pred,
    pred_all
  )

  ggplot(
    pred_combined %>% filter(!is.na(.data[[feature]]) & !is.na(PRED_CLASS)),
    aes(x = if (is_categorical) as.factor(.data[[feature]]) else .data[[feature]])
  ) +
    {
      if (is_categorical) {
        geom_bar(
          fill = "skyblue",
          color = "black",
          stat = "count"
        )
      } else {
        geom_histogram(
          bins = 50,
          fill = "skyblue",
          color = "black"
        )
      }
    } +
    facet_wrap(~ PRED_CLASS, nrow = 2, ncol = 3, scales = "free_y") +
    labs(
      title = paste0("Distribution of ", label, " by Predicted Probability"),
      x = label,
      y = "Frequency"
    ) +
    plot_theme
}

# Most important features according to shap values
feature_cols <- c(
  "BIRTH_YEAR_DOC", "BIRTH_YEAR_PAT", "AGE_AT_VISIT_PAT", "MEAN_YEARLY_PRESCRIPTIONS_DOC", "AGE_AT_VISIT_DOC", "MEAN_YEARLY_PRESCRIPTIONS_PAT",
  "HOME_REGION_DOC_Uusimaa", "MEAN_YEARLY_DIAGNOSES_DOC", "LANGUAGE_DOC_fi", "MEAN_YEARLY_DIAGNOSES_PAT", "GOT_ATC_J01_PAT", "MONTH",
  "HOME_REGION_DOC_Pirkanmaa", "LANGUAGE_DOC_ru", "WEEKDAY", "LANGUAGE_DOC_other", "GOT_ATC_R01_DOC", "SPECIALTY_General_Medicine", "SEX_DOC_male"
)

feature_means <- pred %>%
  group_by(PRED_CLASS) %>%
  summarize(across(
    all_of(feature_cols),
    ~ mean(.x, na.rm = TRUE),
    .names = "{.col}_MEAN"
  ))

feature_mean_vars <- pred %>%
  mutate(across(
    all_of(feature_cols),
    ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))
  )) %>%
  group_by(PRED_CLASS) %>%
  summarize(across(
    all_of(feature_cols),
    ~ mean(.x, na.rm = TRUE),
    .names = "{.col}"
  )) %>%
  summarize(across(
    all_of(feature_cols),
    ~ var(.x, na.rm = TRUE),
    .names = "{.col}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = "FEATURE",
    values_to = "SCALED_VARIANCE"
  ) %>%
  arrange(desc(SCALED_VARIANCE))

feature_hist_by_pred_class("BIRTH_YEAR_DOC", "Doctor Birth Year")
feature_hist_by_pred_class("BIRTH_YEAR_PAT", "Patient Birth Year")
feature_hist_by_pred_class("MEAN_YEARLY_PRESCRIPTIONS_DOC", "Doctor Mean Yearly Prescriptions")
feature_hist_by_pred_class("MEAN_YEARLY_PRESCRIPTIONS_PAT", "Patient Mean Yearly Prescriptions")
feature_hist_by_pred_class("HOME_REGION_DOC_Uusimaa", "Doctor Home Region = Uusimaa")
feature_hist_by_pred_class("MEAN_YEARLY_DIAGNOSES_DOC", "Doctor Mean Yearly Diagnoses")
feature_hist_by_pred_class("MEAN_YEARLY_DIAGNOSES_PAT", "Patient Mean Yearly Diagnoses")
