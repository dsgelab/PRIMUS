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

presc_2014  <- fread("/media/volume/.../presc2014.csv")
glimpse(presc_2014)

# get the doctor IDs
valvira <- fread("valvira.csv", encoding = "Latin-1")
doctors <-  fread("/media/volume/Projects/DSGELabProject1/doctors_20250220.csv", header = F)
docs <- valvira %>% filter(FID %in% doctors$V1) %>% select(FID_DOC = "FID", FD_HASH = "FD_HASH_Rekisteröinti..numero") %>% 
    group_by(FID_DOC, FD_HASH) %>% 
    distinct()

presc_2014_slim <- presc_2014 %>% 
    select(FID, ATC_CODE, FD_HASH = "FD_HASH_Rekisteröinti..numero") %>% 
    left_join(docs, by = "FD_HASH") %>%
    filter(!is.na(FID_DOC)) %>%
    select(-FD_HASH) %>% 
    filter(str_starts(ATC_CODE, "C10AA")) %>% 
    group_by(FID_DOC, FID) %>% 
    distinct()
