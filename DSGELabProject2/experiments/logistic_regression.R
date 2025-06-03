library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(pROC)
library(ggplot2)
library(forcats)

setwd("/media/volume/Projects/mikael/ProcessedData")
source("/media/volume/Projects/mikael/utils.R")
set.seed(123)

diagnosis_file = get_latest_file("J069Diagnoses")
prescription_file = get_latest_file("DiagnosesConnectedtoPrescriptions_J069")
doctor_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
patient_file = "/media/volume/Data/Data_THL_2698_14.02.00_2023/DVV/FD_2698_Tulokset_2024-04-09_HY.csv"

diagnosis <- fread(diagnosis_file) %>% as_tibble()
prescription <- fread(prescription_file) %>% as_tibble()
doctor <- fread(doctor_file) %>% as_tibble()
patient <- fread(patient_file) %>% as_tibble()

# Dataset with doctor information
df <- diagnosis %>%
  inner_join(doctor, by = c("DOCTOR_ID" = "FID")) %>%
  rename(D_BIRTH_DATE = BIRTH_DATE, D_SEX = SEX) %>%
  mutate(D_BIRTH_DATE = ymd(D_BIRTH_DATE)) %>%
  mutate(PRESCRIBED = as.integer(PATIENT_ID %in% prescription$PATIENT_ID)) %>%
  mutate(D_AGE = as.numeric(difftime(VISIT_DATE, D_BIRTH_DATE, units = "days") / 365.25)) %>%
  mutate(SPECIALTY = fct_lump_min(SPECIALTY, min = 10, other_level = "other"))

# Dataset with patient information
df2 <- diagnosis %>%
    inner_join(patient, by = c("PATIENT_ID" = "FID")) %>%
    rename(P_BIRTH_DATE = `Syntymä-päivä`, P_SEX = `Suku-.puoli`) %>%
    mutate(P_BIRTH_DATE = ymd(P_BIRTH_DATE)) %>%
    mutate(PRESCRIBED = as.integer(PATIENT_ID %in% prescription$PATIENT_ID)) %>%
    mutate(P_AGE = as.numeric(difftime(VISIT_DATE, P_BIRTH_DATE, units = "days") / 365.25))


dataset_size <- function(df, ...) {
    clean_df_size = nrow(df %>% drop_na(...))
    n_diagnoses = nrow(diagnosis)
    df_diagnosis_ratio = sprintf("%.1f%%", clean_df_size / n_diagnoses * 100)
    print(paste0("Dataset size: ", clean_df_size, ", ", df_diagnosis_ratio, " of diagnoses"))
    prescribed = sum(df$PRESCRIBED)
    prescribed_ratio = sprintf("%.1f%%", prescribed / clean_df_size * 100)
    print(paste0("Number of patients with prescription: ", prescribed, ", ", prescribed_ratio, " of the dataset"))
}
dataset_size(df, D_BIRTH_DATE, D_SEX)

# Account for imbalance in data
model_weights <- function(df) {
    class_counts = table(df$PRESCRIBED)
    imbalance_ratio = round(max(class_counts) / min(class_counts))
    w = ifelse(df$PRESCRIBED == 0, 1, imbalance_ratio)
    w
}

weights <- model_weights(df)
model <- glm(PRESCRIBED ~ D_AGE + D_SEX, data = df, family = "binomial", weights = weights)
summary(model)

regression_results <- function(model, df) {
    probabilities = predict(model, type = "response")
    predicted_class = ifelse(probabilities > 0.5, 1, 0)
    conf_matrix = table(Predicted = predicted_class, Actual = df$PRESCRIBED)
    precision = conf_matrix[2, 2] / (conf_matrix[2, 2] + conf_matrix[2, 1])
    recall = conf_matrix[2, 2] / (conf_matrix[2, 2] + conf_matrix[1, 2])
    accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
    print(paste("Precision:", precision, "Recall:", recall, "Accuracy:", accuracy))

    conf_df = as.data.frame(as.table(conf_matrix))
    ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
        geom_tile(color = "white") +
        geom_text(aes(label = Freq), color = "white", size = 8) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14)) +
        labs(title = "Confusion Matrix",
             subtitle = paste("Accuracy:", round(accuracy, 3))) +
        coord_fixed()
}
regression_results(model, df)

dataset_size(df2, P_AGE, P_SEX)
weights2 <- model_weights(df2)
model2 <- glm(PRESCRIBED ~ P_AGE + P_SEX, data = df2, family = "binomial", weights = weights2)
summary(model2)
regression_results(model2, df2)

model3 <- glm(PRESCRIBED ~ D_AGE + D_SEX + SPECIALTY, data = df, family = "binomial", weights = weights)
summary(model3)
