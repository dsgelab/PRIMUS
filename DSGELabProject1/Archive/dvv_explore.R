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


dvv1 <- fread("DVV/dvv1.csv")
glimpse(dvv1)
summary(dvv1)
dvv1 %>% 
    summarise(across(everything(), list(n_distinct = ~n_distinct(.), 
    pct_missing = ~mean(is.na(.)) * 100)))


dvv2  <- fread("DVV/dvv2.csv")
glimpse(dvv2)
summary(dvv2)
dvv2 %>% 
    summarise(across(everything(), list(n_distinct = ~n_distinct(.), 
    pct_missing = ~mean(is.na(.)) * 100)))

dvv3  <- fread("DVV/dvv3.csv")
glimpse(dvv3)
summary(dvv3)
dvv3 %>% 
    summarise(across(everything(), list(n_distinct = ~n_distinct(.), 
    pct_missing = ~mean(is.na(.)) * 100)))

dvv4  <- fread("DVV/dvv4.csv")
glimpse(dvv4)
summary(dvv4)
dvv4 %>% 
    summarise(across(everything(), list(n_distinct = ~n_distinct(.), 
    pct_missing = ~mean(is.na(.)) * 100)))

dvv5  <- fread("DVV/dvv5.csv")
glimpse(dvv5)
summary(dvv5)
dvv5 %>% 
    summarise(across(everything(), list(n_distinct = ~n_distinct(.), 
    pct_missing = ~mean(is.na(.)) * 100)))