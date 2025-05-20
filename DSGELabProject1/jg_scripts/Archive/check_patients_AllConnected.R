library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(lubridate)
library(stringr)
library(data.table)
library(R.utils)

mpath <- "/media/volume/Projects/jg/"
setwd(mpath)

# Load the data
dp_prescpurch <- fread("AllConnected_prescpurch_20250327_173343.csv")
glimpse(dp_prescpurch)

dp_prescpurch <- dp_prescpurch %>%
    rename(CODE = ATC_CODE) 
dp_prescpurch <- dp_prescpurch %>%
    mutate(
        DATE = ifelse(
            Source == "Prescription", 
            PRESCRIPTION_DATE, 
            PURCHASE_DATE
        )
    ) %>%
    mutate(DATE = as.IDate(DATE, format = "%Y-%m-%d"))

tmp_A <- dp_prescpurch %>% filter(PATIENT_ID == "XXXXXXX") %>% arrange(CODE, DATE)


plot_A <- tmp_A %>% filter(CODE == "A02BC01") %>%
    ggplot(aes(x = DATE, y = Source, color = Source)) +
    geom_point() +
    scale_y_discrete(limits = c("Prescription", "Purchase")) +
    scale_x_date(limits = as.Date(c("2014-01-01", "2023-12-31"))) + # Limit x-axis
    labs(
        title = "Timeline of Prescriptions and Purchases (tmp_A)",
        x = "Date",
        y = "Register"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

plot_B <- tmp_A %>%
    filter(CODE == "A02BC01" & Source == "Purchase") %>%
    ggplot(aes(x = PRESCRIPTION_DATE, y = Source)) +
    geom_point(color = "green") +
    scale_y_discrete(
        limits = c("Purchase"),
        labels = c("Purchase" = "Imputed Prescription")
    ) +
    scale_x_date(
        limits = as.Date(c("2014-01-01", "2023-01-01"))
    ) + # Limit x-axis
    labs(
        title = "Timeline of Imputed Prescriptions (from Purchases)",
        x = "Date",
        y = "Register"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

library(patchwork)
plot_A / plot_B

tmp_A %>% filter(DATE > ymd("2020-01-01"))



######
plot_patient_code <- function(data, patient_id, code) {
    tmp_A <- data %>% 
        filter(PATIENT_ID == patient_id) %>%  # nolint
        arrange(CODE, DATE) # nolint
    
    plot_A <- tmp_A %>% 
        filter(CODE == code) %>% # nolint
        ggplot(aes(x = DATE, y = Source, color = Source)) + # nolint
        geom_point() +
        scale_y_discrete(limits = c("Prescription", "Purchase")) +
        scale_x_date(limits = as.Date(c("2014-01-01", "2023-01-01"))) + # Limit x-axis
        labs(
            title = paste("Timeline of Prescriptions and Purchases (", patient_id, ", ATC = ", code, ")", sep = ""),
            x = "Date",
            y = "Register"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    
    plot_B <- tmp_A %>%
        filter(CODE == code & Source == "Purchase") %>% # nolint
        ggplot(aes(x = PRESCRIPTION_DATE, y = Source)) + # nolint
        geom_point(color = "green") +
        scale_y_discrete(
            limits = c("Purchase"),
            labels = c("Purchase" = "Imputed Prescription")
        ) +
        scale_x_date(
            limits = as.Date(c("2014-01-01", "2023-01-01"))
        ) + # Limit x-axis
        labs(
            #title = paste("Timeline of Imputed Prescriptions from Purchases (", patient_id, ", ATC = ", code, ")", sep = ""),
            x = "Date",
            y = "Register"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    
    library(patchwork)
    plot_A / plot_B
}

plot_all_codes_for_patient <- function(data, patient_id, num_codes = NULL) {
    tmp_A <- data %>% 
        filter(PATIENT_ID == patient_id) %>%  # nolint
        filter(CODE != "") %>%  # nolint
        group_by(CODE) %>% 
        mutate(CODE_COUNT = n()) %>% 
        ungroup() %>% 
        arrange(desc(CODE_COUNT), CODE, DATE) # nolint
    
    unique_codes <- unique(tmp_A$CODE)
    
    if (!is.null(num_codes)) {
        unique_codes <- head(unique_codes, num_codes)
    }
    
    plots <- map(unique_codes, function(code) {
        plot_patient_code(data, patient_id, code)
    })
    
    wrap_plots(plots, ncol = 2) # nolint
}

# Example usage:
plot_all_codes_for_patient(dp_prescpurch, "XXXXXXX", 2)


# Check how many imputed prescriptions capture actual prescriptions after 2015-01-01
df_compare <- dp_prescpurch %>% 
    group_by(PATIENT_ID, DOCTOR_ID, CODE, PRESCRIPTION_DATE, Source) %>% 
    filter(Source == "Prescription" | (Source == "Purchase" & row_number() == 1)) %>% 
    ungroup() %>% 
    select(PATIENT_ID, DOCTOR_ID, CODE, PRESCRIPTION_DATE, Source)

df_compare %>% 
    filter(PRESCRIPTION_DATE >= "2015-01-01") %>% 
    group_by(PATIENT_ID, DOCTOR_ID, CODE) %>% 
    summarise(
        n_prescriptions = sum(Source == "Prescription"),
        n_purchases = sum(Source == "Purchase"),
        .groups = "drop"
    ) %>% 
    filter(n_prescriptions > 0 & n_purchases > 0) %>% 
    summarise(
        total_patients = n(),
        total_prescriptions = sum(n_prescriptions),
        total_purchases = sum(n_purchases)
    )

df_compare %>% 
    filter(PRESCRIPTION_DATE >= "2015-01-01")  %>% 
    group_by(PATIENT_ID, CODE, PRESCRIPTION_DATE) %>%
    filter(Source == "Prescription" | (Source == "Purchase" & DATE == min(DATE[Source == "Purchase"]))) %>% 
    ungroup() %>%
    arrange(PATIENT_ID, CODE, PRESCRIPTION_DATE)

