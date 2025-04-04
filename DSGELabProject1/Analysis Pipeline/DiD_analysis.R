#### Info:
# This script takes as input a list of doctor ids (cases + controls) and two datasets Events.csv and Outcomes.csv 
# It then performs a difference-in-differences analysis based on the input data

#### Libraries:
library(data.table)
library(dplyr)
library(lubridate)
library(fixest)

##### Arguments
args = commandArgs(trailingOnly = TRUE)
doctor_list = args[1]
map_relatives = args[2]
id_cases = args[3]
id_controls = args[4]
events_file = args[5]
outcomes_file = args[6]
outdir = args[7]

#### Main

# Load data
all_cases = fread(id_cases, header = FALSE)$V1
all_controls = fread(id_controls, header = FALSE)$V1
all_doctors = fread(doctor_list, header = FALSE)$V1
map_relatives = fread(map_relatives, header = TRUE)
events = fread(events_file)
outcomes = fread(outcomes_file)

# VERSION 1: only consider doctors with events
cases_doctors = intersect(all_doctors,all_cases)
controls_doctors = intersect(all_doctors,all_controls)

# VERSION 2: consider doctors with relatives
extra_cases <- map_relatives %>% filter(RELATIVE_FID %in% all_cases) %>% select(DOCTOR_ID) %>% unique()
cases_doctors_and_relatives = union(cases_doctors,extra_cases)
controls_doctors_and_relatives = setdiff(all_doctors,cases_doctors_and_relatives)

# VERSION 3: consider only relatives
cases_relatives = map_relatives %>% filter(RELATIVE_FID %in% all_cases) %>% select(DOCTOR_ID) %>% unique()
controls_relatives = map_relatives %>% filter(RELATIVE_FID %in% all_controls) %>% select(DOCTOR_ID) %>% unique()

# report final number of cases and controls for all versions
cat('VERSION 1: only doctors:\n')
cat(paste0('cases : ',length(cases_doctors),'\n'))
cat(paste0('controls : ',length(controls_doctors),'\n'))
cat('VERSION 2: doctors and relatives:\n')
cat(paste0('cases : ',length(cases_doctors_and_relatives),'\n'))
cat(paste0('controls : ',length(controls_doctors_and_relatives),'\n'))
cat('VERSION 3: only relatives:\n')
cat(paste0('cases : ',length(cases_relatives),'\n'))
cat(paste0('controls : ',length(controls_relatives),'\n'))

counter = 1
for (id_list in list(
    list(cases_doctors, controls_doctors),
    list(cases_doctors_and_relatives, controls_doctors_and_relatives),
    list(cases_relatives, controls_relatives)
)) {
    cat(paste0('VERSION ', counter, ' results:\n'))
    if (length(id_list[[1]]) <= 1 | length(id_list[[2]]) <= 1) {
        cat('Not enough cases or controls available\n')
        counter = counter + 1
        next
    }
    
    # Perform DiD analysis
    outcomes_new = outcomes %>%
        mutate(
            YEAR = year(ymd(DATE)),
            EVENT = ifelse(DOCTOR_ID %in% id_list[[1]], 1, 0)
        ) %>%
        group_by(DOCTOR_ID, YEAR) %>%
        summarise(Y = n(), .groups = 'drop')
    
    # Report number of doctors with outcomes
    N_CASES_OUT = length(intersect(id_list[[1]], unique(outcomes_new$DOCTOR_ID)))
    N_CONTROLS_OUT = length(intersect(id_list[[2]], unique(outcomes_new$DOCTOR_ID)))
    cat(paste0('cases with outcome info (%): ', N_CASES_OUT * 100 / length(id_list[[1]]), '\n'))
    cat(paste0('controls with outcome info (%): ', N_CONTROLS_OUT * 100 / length(id_list[[2]]), '\n'))
    
    # Add info about events (POST=1 if outcome happened after the event)
    tmp = events[, c("PATIENT_ID", "DATE")] %>% rename('DOCTOR_ID'='PATIENT_ID') 
    df_merged = left_join(outcomes_new, tmp, by = "DOCTOR_ID")
    df_merged$POST = if_else(is.na(df_merged$DATE) | df_merged$YEAR <= year(ymd(df_merged$DATE)), 0, 1)
    
    # DiD analysis
    model = fixest::feols(Y ~ YEAR + POST + YEAR * POST | DOCTOR_ID, data = df_merged)
    results = data.frame(summary(model)$coeftable)
    
    # Save results
    write.csv(results, file = paste0(outdir, '/DiD_results_VERSION', counter, '.csv'), row.names = FALSE)
    
    counter = counter + 1
}
