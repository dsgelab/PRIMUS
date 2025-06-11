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
library(readxl)

mpath <- "/media/volume/Prokect/DSGE_Lab_Project1/"
setwd(mpath)

doctors <- fread("doctor_characteristics_20250520.csv")
glimpse(doctors)
summary(doctors)
doctors %>%
    summarise(across(everything(), list(
        n_distinct = ~ n_distinct(.),
        pct_missing = ~ mean(is.na(.)) * 100
    )))
