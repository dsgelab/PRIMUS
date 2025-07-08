library(data.table)
library(dplyr)
library(ggplot2)
setwd("/media/volume/Projects/mikael/XGBoost/results")
source("/media/volume/Projects/mikael/utils.R")

pos_rate <- 0.061
PRED_CLASS_LABELS <- c(
  "Prescribed, 0 <= pred <= 0.2",
  "Prescribed, 0.2 < pred <= 0.8",
  "Prescribed, 0.8 < pred <= 1",
  paste0("Not Prescribed, pred <= ", pos_rate),
  paste0("Not Prescribed, pred > ", pos_rate)
)

pred <- fread("xgb_predictions_2025-07-07-0657.csv") %>% as_tibble()
pred <- pred %>%
  mutate(
    PRED_CLASS = factor(case_when(
      PRESCRIBED == 1 & XGB_PRED <= 0.2 ~ PRED_CLASS_LABELS[1],
      PRESCRIBED == 1 & XGB_PRED > 0.2 & XGB_PRED <= 0.8 ~ PRED_CLASS_LABELS[2],
      PRESCRIBED == 1 & XGB_PRED > 0.8 & XGB_PRED <= 1 ~ PRED_CLASS_LABELS[3],
      PRESCRIBED == 0 & XGB_PRED <= pos_rate ~ PRED_CLASS_LABELS[4],
      PRESCRIBED == 0 & XGB_PRED > pos_rate ~ PRED_CLASS_LABELS[5],
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
        alpha = 0.4,
    ) +
    scale_fill_manual(
        values = c("0" = "#f4c0bd", "1" = "#91dddf"),
        labels = c("0" = "Did not presribe", "1" = "Prescribed")
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

  ggplot(
    pred %>% filter(!is.na(.data[[feature]])),
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
      title = paste0("Distribution of ", label, " by True Label and Predicted Probability"),
      x = label,
      y = "Frequency"
    ) +
    plot_theme
}

feature_hist_by_pred_class("AGE_DOC", "Doctor Age")
feature_hist_by_pred_class("AGE_PAT", "Patient Age")
feature_hist_by_pred_class("SEX_DOC_female", "Doctor Sex = Female")
feature_hist_by_pred_class("SEX_PAT_female", "Patient Sex = Female")
feature_hist_by_pred_class("HAD_O_DOC", "Doctor had O Diagnosis")
feature_hist_by_pred_class("SPECIALTY_No_Specialty", "Doctor has no specialty")
feature_hist_by_pred_class("GOT_G_DOC", "Doctor was prescribed G")

