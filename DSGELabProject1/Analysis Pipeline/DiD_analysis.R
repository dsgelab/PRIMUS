#### Info:
# This script takes as input a list of doctor ids (cases + controls) and two datasets Events.csv and Outcomes.csv
# It then performs a difference-in-differences analysis based on the input data

#### Libraries:
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(fixest)
library(ggplot2)

##### Arguments
args <- commandArgs(trailingOnly = TRUE)
doctor_list <- args[1]
map_relatives <- args[2]
id_cases <- args[3]
id_controls <- args[4]
events_file <- args[5]
outcome_code <- args[6]
outdir <- args[7]

# Functions
create_pre_post_dummies <- function(data) {
    years <- 2014:2022
    for (i in 1:length(years)) {
        t <- i # T years before/after the event
        pre_name <- paste0("PRE", t)
        post_name <- paste0("POST", t)
        data[[pre_name]] <- ifelse(!is.na(data$EVENT_YEAR) & data$YEAR == data$EVENT_YEAR - t, 1, 0)
        data[[post_name]] <- ifelse(!is.na(data$EVENT_YEAR) & data$YEAR == data$EVENT_YEAR + t, 1, 0)
    }
    return(data)
}

#### Main

# Load data
all_cases <- fread(id_cases, header = FALSE)$V1
all_controls <- fread(id_controls, header = FALSE)$V1
all_doctors <- fread(doctor_list, header = FALSE)$V1
map_relatives <- fread(map_relatives, header = TRUE)
events <- fread(events_file)
outcomes <- fread(outcomes_file)
outcomes <- outcomes[outcomes[, .I[which.min(DATE)], by = .(DOCTOR_ID, PATIENT_ID, CODE)]$V1] # use only first prescriptions

# VERSION 1: only consider doctors with events
cases_doctors <- intersect(all_doctors, all_cases)
controls_doctors <- intersect(all_doctors, all_controls)

# VERSION 2: consider doctors with relatives
extra_cases <- map_relatives %>%
    filter(RELATIVE_FID %in% all_cases) %>%
    select(DOCTOR_ID) %>%
    unique()
cases_doctors_and_relatives <- union(cases_doctors, extra_cases)
controls_doctors_and_relatives <- setdiff(all_doctors, cases_doctors_and_relatives)

# VERSION 3: consider only relatives
cases_relatives <- map_relatives %>%
    filter(RELATIVE_FID %in% all_cases) %>%
    select(DOCTOR_ID) %>%
    unique()
controls_relatives <- map_relatives %>%
    filter(RELATIVE_FID %in% all_controls) %>%
    select(DOCTOR_ID) %>%
    unique()

# report final number of cases and controls for all versions
cat("VERSION 1: only doctors:\n")
cat(paste0("cases : ", length(cases_doctors), "\n"))
cat(paste0("controls : ", length(controls_doctors), "\n"))
cat("VERSION 2: doctors and relatives:\n")
cat(paste0("cases : ", length(cases_doctors_and_relatives), "\n"))
cat(paste0("controls : ", length(controls_doctors_and_relatives), "\n"))
cat("VERSION 3: only relatives:\n")
cat(paste0("cases : ", length(cases_relatives), "\n"))
cat(paste0("controls : ", length(controls_relatives), "\n"))

counter <- 1
for (id_list in list(
    list(cases_doctors, controls_doctors),
    list(cases_doctors_and_relatives, controls_doctors_and_relatives),
    list(cases_relatives, controls_relatives)
)) {
    cat(paste0("VERSION ", counter, " results:\n"))
    if (length(id_list[[1]]) <= 1 || length(id_list[[2]]) <= 1) {
        cat("Not enough cases or controls available\n")
        counter <- counter + 1
        next
    }

    # Perform DiD analysis
    cat("Performing DiD analysis\n")
    system.time({
        outcomes_new <- outcomes %>%
            filter(!is.na(DATE)) %>%
            mutate(
                YEAR = as.numeric(format(DATE, "%Y")),
                EVENT = ifelse(DOCTOR_ID %in% id_list[[1]], 1, 0)
            ) %>%
            group_by(DOCTOR_ID, YEAR) %>%
            summarise(
                Ni = sum(grepl(paste0("^", outcome_code), CODE)),
                N = n(),
                .groups = "drop"
            ) %>%
            complete(DOCTOR_ID, YEAR = seq(2014, 2022, by = 1), fill = list(Ni = 0, N = 0)) %>%
            mutate(Y = ifelse(N == 0, NA, Ni / N))
    })

    # Report number of doctors with outcome of interest
    doctors_with_outcomes <- outcomes_new %>%
        group_by(DOCTOR_ID) %>%
        summarise(has_outcome = any(Y != 0, na.rm = TRUE)) %>%
        filter(has_outcome) %>%
        pull(DOCTOR_ID)
    N_CASES_OUT <- length(intersect(id_list[[1]], doctors_with_outcomes))
    N_CONTROLS_OUT <- length(intersect(id_list[[2]], doctors_with_outcomes))
    cat(paste0("cases with outcome info (%): ", N_CASES_OUT * 100 / length(id_list[[1]]), "\n"))
    cat(paste0("controls with outcome info (%): ", N_CONTROLS_OUT * 100 / length(id_list[[2]]), "\n"))

    # Add info about events
    tmp <- events[, c("PATIENT_ID", "DATE")] %>% rename("DOCTOR_ID" = "PATIENT_ID")
    df_merged <- left_join(outcomes_new, tmp, by = "DOCTOR_ID")
    df_merged <- df_merged %>%
        mutate(
            EVENT = if_else(!is.na(DATE), 1, 0),
            EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
            TIME_SINCE_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, YEAR - EVENT_YEAR)
        ) %>%
        filter(is.na(DATE) | as.Date(DATE) >= as.Date("2014-01-01")) %>%
        select(-DATE)
    df_merged <- create_pre_post_dummies(df_merged)

    # DiD analysis
    dummy_vars <- grep("^(PRE|POST)\\d+$", names(df_merged), value = TRUE)
    model_formula <- as.formula(paste("Y ~ YEAR + EVENT +", paste(dummy_vars, collapse = " + "), "| DOCTOR_ID"))
    model <- fixest::feols(model_formula, data = df_merged)
    results <- data.frame(summary(model)$coeftable)
    # Save results
    write.csv(results, file = paste0(outdir, "/DiD_results_VERSION", counter, ".csv"), row.names = FALSE)

    # Plot 1 : distribution of events:
    tmp <- tmp[tmp$DATE >= as.Date("2014-01-01"), ]
    tmp$YEAR <- as.numeric(format(tmp$DATE, "%Y"))
    p1 <- barplot(table(tmp$YEAR), xlab = "Year", ylab = "Number of events")

    # Plot 2 : DiD results
    results$time[grepl("PRE", rownames(results))] <- -as.numeric(gsub("PRE", "", rownames(results)[grepl("PRE", rownames(results))]))
    results$time[grepl("POST", rownames(results))] <- as.numeric(gsub("POST", "", rownames(results)[grepl("POST", rownames(results))]))
    p2 <- ggplot(results, aes(x = time, y = Estimate)) +
        geom_point() +
        geom_errorbar(aes(ymin = Estimate - 1.96 * Std..Error, ymax = Estimate + 1.96 * Std..Error), width = 0.2) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        theme_minimal() +
        labs(x = "Years from Event", y = "Coefficient")

    # save results
    ggsave(paste0(outdir, "/EventCounts_VERSION", counter, ".png"), plot = p1, width = 14, height = 8, dpi = 300)
    ggsave(paste0(outdir, "/DiD_plot_VERSION", counter, ".png"), plot = p2, width = 14, height = 8, dpi = 300)

    counter <- counter + 1
}
