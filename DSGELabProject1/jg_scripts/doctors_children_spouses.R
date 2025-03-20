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

mpath <- "C:/Users/.../"
setwd(mpath)

doctors  <- fread('/media/volume/Projects/DSGELabProject1/doctors_20250220.csv', header = F)
glimpse(doctors)

dvv_children <- fread("DVV/dvv_children.csv")
glimpse(dvv_children)

dvv_children <- dvv_children %>% 
    select(FID = 'FID_Kantahenkilön.henkilötunnus', RELATIVE_FID = 'FID_Sukulaisen.henkilötunnus') %>% 
    filter(FID %in% doctors$V1) %>%
    filter(!is.na(RELATIVE_FID)) %>% 
    mutate(RELATIVE_TYPE = 'CHILD')

# Sanity check
all(dvv_children$FID %in% doctors$V1)

dvv_spouses <- fread("DVV/dvv_spouses.csv")
glimpse(dvv_spouses)

dvv_spouses <- dvv_spouses %>% 
    select(FID = 'FID_Kantahenkilön.henkilötunnus', RELATIVE_FID = 'FID_Puolison.henkilötunnus') %>% 
    filter(FID %in% doctors$V1) %>% 
    filter(!is.na(RELATIVE_FID)) %>% 
    mutate(RELATIVE_TYPE = 'SPOUSE')

# Sanity check
all(dvv_spouses$FID %in% doctors$V1)

# Combine the columns into a single data frame
doctors_and_family <- bind_rows(
    tibble(FID = doctors$V1),
    tibble(FID = dvv_children$CHILDREN_FID),
    tibble(FID = dvv_spouses$SPOUSE_FID)
) %>% distinct()

glimpse(doctors_and_family)

doctors_and_family_type  <- bind_rows(
    dvv_children,
    dvv_spouses 
) %>% 
select("DOCTOR_ID" = "FID", RELATIVE_FID, RELATIVE_TYPE)

# Save the IDs
fwrite(dvv_children, "doctors_and_children_20250305.csv", col.names = F)
fwrite(dvv_spouses, "doctors_and_spouses_20250305.csv", col.names = F)
fwrite(doctors_and_family, "doctors_and_spouses+children_20250305.csv", col.names = F)
fwrite(doctors_and_family_type, "doctors_and_relative_20250305.csv")
