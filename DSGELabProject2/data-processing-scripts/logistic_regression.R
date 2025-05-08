library(dplyr)
library(lubridate)
library(pROC)
library(ggplot2)

set.seed(123)

diagnosis_file = "/media/volume/Projects/mikael/J069Diagnoses_20250507.csv"
prescription_file = "/media/volume/Projects/mikael/DiagnosesConnectedtoPrescriptions_J069_20250507.csv"
doctor_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_wlongest_Specialty_20250220.csv"

diagnosis <- fread(diagnosis_file) %>% as_tibble()
prescription <- fread(prescription_file) %>% as_tibble()
doctor <- fread(doctor_file) %>% as_tibble()

df <- diagnosis %>%
  inner_join(doctor, by = c("DOCTOR_ID" = "FID")) %>%
  rename(D_BIRTH_DATE = BIRTH_DATE, D_SEX = SEX) %>%
  mutate(D_BIRTH_DATE = ymd(D_BIRTH_DATE)) %>%
  mutate(PRESCRIBED = as.integer(PATIENT_ID %in% prescription$PATIENT_ID)) %>%
  mutate(D_AGE = as.numeric(difftime(DIAGNOSIS_DATE, D_BIRTH_DATE, units = "days") / 365.25))

df_size <- nrow(df)
n_diagnoses <- nrow(diagnosis)
df_diagnosis_ratio <- sprintf("%.1f%%", df_size / n_diagnoses * 100)
print(paste0("Dataset size: ", df_size, ", ", df_diagnosis_ratio, " of diagnoses"))
prescribed <- sum(df$PRESCRIBED)
prescribed_ratio <- sprintf("%.1f%%", prescribed / df_size * 100)
print(paste0("Number of patients with prescription: ", prescribed, ", ", prescribed_ratio, " of the dataset"))

# Account for imbalance in data
class_counts <- table(df$PRESCRIBED)
imbalance_ratio <- round(max(class_counts) / min(class_counts))
model_weights <- ifelse(df$PRESCRIBED == 0, 1, imbalance_ratio)
model <- glm(PRESCRIBED ~ D_AGE + D_SEX, data = df, family = "binomial", weights = model_weights)
summary(model)

probabilities <- predict(model, type = "response")
predicted_class <- ifelse(probabilities > 0.5, 1, 0)
conf_matrix <- table(Predicted = predicted_class, Actual = df$PRESCRIBED)
precision <- conf_matrix[2, 2] / (conf_matrix[2, 2] + conf_matrix[2, 1])
recall <- conf_matrix[2, 2] / (conf_matrix[2, 2] + conf_matrix[1, 2])
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
print(paste("Precision:", precision, "Recall:", recall, "Accuracy:", accuracy))

conf_df <- as.data.frame(as.table(conf_matrix))
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
