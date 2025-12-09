library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(lubridate)
library(stringr)
library(data.table)
library(R.utils)
library(readxl)
# .libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(Epi)

# Load all IDs
dvv <- fread("/path/to/dvv.csv") 

# Preprocess the data
dvv <- dvv %>% rename(ID = FID,
                     BIRTH_DATE = `Syntymä-päivä`,
                     DEATH_DATE = Kuolinpäivä,
                     SEX = `Suku-.puoli`) %>% 
                select(ID, SEX, BIRTH_DATE, DEATH_DATE) %>% 
                mutate(BIRTH_DATE = as.Date(as.character(BIRTH_DATE), format = "%Y%m%d"),
                       DEATH_DATE = as.Date(as.character(DEATH_DATE), format = "%Y%m%d"))

dvv <- dvv %>%
    mutate(
        is_doctor = ifelse(ID %in% doctor_IDs$V1, 1, 0),
        age_end_of_FU = round(as.numeric(difftime(
            dplyr::if_else(is.na(DEATH_DATE), as.Date("2022-12-31"), DEATH_DATE),
            BIRTH_DATE,
            units = "days")) / 365.25, 2),
        DEATH = ifelse(is.na(DEATH_DATE), 0, 1)
    )

### Create ref population ###

# Build ENTRY_DATE and EXIT_DATE
END_OF_DATA  <- as.Date("2022-12-31")
START_OF_FU  <- as.Date("1998-01-01")

pop_df <- dvv %>%       
  mutate(
    ENTRY_DATE = pmax(START_OF_FU, BIRTH_DATE),
    EXIT_DATE  = pmin(
      ifelse(DEATH == 1, DEATH_DATE, END_OF_DATA),
      END_OF_DATA
    )
  )

# Create a Lexis object (age as timescale)
L_pop <- Lexis(
  entry = list(
    age = as.numeric(ENTRY_DATE - BIRTH_DATE) / 365.25
  ),
  exit = list(
    age = as.numeric(EXIT_DATE  - BIRTH_DATE) / 365.25
  ),
  exit.status = factor(ifelse(DEATH == 1, "dead", "alive")),
  id   = ID,
  data = pop_df
)

# Split by age bands
age_breaks <- seq(0, 100, by = 5)

L_pop_split <- splitLexis(
  L_pop,
  breaks     = age_breaks,
  time.scale = "age"
)

# Aggregate person-time by SEX × age band
std_pop <- L_pop_split %>%
  mutate(
    age_band = cut(
      age,
      breaks = age_breaks,
      right  = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  group_by(SEX, age_band) %>%
  summarise(
    pop_pyrs = sum(lex.dur),   # total PY in that SEX × age band
    .groups  = "drop"
  )

std_pop <- std_pop %>% filter(!is.na(age_band))

# Add weights
std_pop <- std_pop %>%
  mutate(weight = pop_pyrs / sum(pop_pyrs))

# Trim to 20-60 years olds
std_pop_20_60 <- std_pop %>%
  mutate(age_lower = as.numeric(sub("\\[([0-9]+),.*", "\\1", age_band))) %>%
  filter(age_lower >= 20 & age_lower < 60) %>%
  select(-age_lower) %>%
  mutate(weight = pop_pyrs / sum(pop_pyrs))   # recalculate weights to sum up to 1

# Save reference population
fwrite(std_pop, "/path/to/ref_population.csv")
fwrite(std_pop_20_60, "/path/to/ref_population_20_60.csv")

# Save DVV (ID/BIRTH_DATE) (for other uses)
fwrite(dvv %>% select(ID, BIRTH_DATE), "/path/to/dvv_processed.csv")