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

mpath <- "C:/Users/.../valvira/"
setwd(mpath)

valvira <- fread("valvira.csv", encoding = "Latin-1")

primus_doctors <- fread("/media/volume/Projects/DSGELabProject1/doctors_20250220.csv",
    header = F, 
    col.names = c("FID")
)

valvira <- valvira %>% filter(FID %in% primus_doctors$FID)

valvira <- valvira %>%
    mutate(
        START_DATE = as.Date(Ammattioikeus.voimassa.alkupäivämäärä, format = "%d.%m.%Y"),
        END_DATE = as.Date(Ammattioikeus.voimassa.loppupäivämäärä, format = "%d.%m.%Y")
    ) %>%
    select(
        -Ammattioikeus.voimassa.alkupäivämäärä, -Ammattioikeus.voimassa.loppupäivämäärä,
        -FD_HASH_Rekisteröinti..numero
    )



# spec_dict <- read_excel("/media/volume/Projects/jg/specialty_dict.xlsx", sheet = 1)
# spec_dict <- as.data.table(spec_dict)
# head(spec_dict)

# spec_dict <- spec_dict %>% filter(CODEVALUE %in% valvira$Tutkinto_Koodi)

# spec_dict <- spec_dict %>% arrange(CODEVALUE)
# write.xlsx(spec_dict, file = "/media/volume/Projects/jg/filtered_specialty_dict.xlsx", rowNames = FALSE)

# # manually currate the specialty list in Excel

spec_dict  <- read_excel("/media/volume/Projects/jg/condensed_specialty_dict.xlsx", sheet = 1)

valvira <- valvira %>%
    mutate(Tutkinto_Koodi = substr(Tutkinto_Koodi, 1, 5)) %>%
    left_join(spec_dict %>% select(-COMPRISED), by = c("Tutkinto_Koodi" = "CODEVALUE"))



set.seed(123) # For reproducibility

# Adjust the x-axis labels to avoid overlap
theme_set(theme_minimal(base_size = 12))
theme_update(axis.text.x = element_text(angle = 45, hjust = 1))

# Panel A: Counts of doctors across specialties
panel_a <- valvira %>%
    group_by(LABEL_EN) %>%
    summarise(count = n_distinct(FID)) %>%
    ggplot(aes(x = reorder(LABEL_EN, -count), y = count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Counts of Doctors Across Specialties", x = "Specialty", y = "Count of Doctors") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5))

# Panel B: Counts of each doctor's number of specialties over their time
panel_b <- valvira %>%
    group_by(FID) %>%
    summarise(num_specialties = n_distinct(LABEL_EN)) %>%
    ggplot(aes(x = num_specialties)) +
    geom_bar(fill = "steelblue") +
    labs(title = "Counts of Each Doctor's Number of Specialties", x = "Number of Specialties", y = "Count of Doctors") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5))

# Panel C: Visualize the jumps of doctors between specialties for a subset of 10 doctors
subset_doctors <- sample(unique(valvira$FID), 10)
panel_c <- valvira %>%
    filter(FID %in% subset_doctors) %>%
    arrange(FID, START_DATE) %>%
    group_by(FID) %>%
    mutate(next_specialty = lead(LABEL_EN)) %>%
    filter(!is.na(next_specialty) & LABEL_EN != next_specialty) %>%
    ggplot(aes(x = LABEL_EN, xend = next_specialty, y = 1, yend = 1, color = as.factor(FID))) +
    geom_curve(curvature = 0.2) +
    labs(
        title = "Jumps of Doctors Between Specialties (Subset of 10 Doctors)",
        x = "Specialty",
        y = ""
    ) +
    theme_minimal(base_size = 15) +
    theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)
    )

# Panel C_1: Visualize the jumps of doctors between specialties for a subset of 10 doctors with separated trajectories
panel_c1 <- valvira %>%
    filter(FID %in% subset_doctors) %>%
    arrange(FID, START_DATE) %>%
    group_by(FID) %>%
    mutate(next_specialty = lead(LABEL_EN)) %>%
    filter(!is.na(next_specialty) & LABEL_EN != next_specialty) %>%
    ggplot(aes(x = LABEL_EN, xend = next_specialty, y = as.factor(FID), yend = as.factor(FID), color = as.factor(FID))) +
    geom_curve(curvature = 0.05) +
    labs(
        title = "Jumps of Doctors Between Specialties (Subset of 10 Doctors, Separated Trajectories)",
        x = "Specialty",
        y = "Doctor ID"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)
    )

# Panel D: Aggregate the trajectories of all doctors
# Panel D: Aggregate the trajectories of doctors between specialties over time
panel_d <- valvira %>%
    arrange(FID, START_DATE) %>%
    group_by(LABEL_EN, next_specialty = lead(LABEL_EN)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    filter(!is.na(next_specialty) & LABEL_EN != next_specialty) %>%
    ggplot(aes(x = LABEL_EN, xend = next_specialty, y = count, yend = count)) +
    geom_segment(arrow = arrow(length = unit(0.2, "cm")), size = 1) +
    labs(
        title = "Aggregated Trajectories of Doctors Between Specialties Over Time",
        x = "Specialty",
        y = "Count of Transitions",
        color = "Doctor ID"
    ) +
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

# Combine the panels into one plot
library(gridExtra)
grid.arrange(panel_a, panel_b, panel_c, panel_d, ncol = 1)

# Prepare data for Sankey diagram
sankey_data <- valvira %>%
    arrange(FID, START_DATE) %>%
    group_by(LABEL_EN, next_specialty = lead(LABEL_EN)) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count >= 5) %>%
    filter(!is.na(next_specialty) & LABEL_EN != next_specialty)

nodes <- data.frame(
    name = unique(c(sankey_data$LABEL_EN, sankey_data$next_specialty))
)
nodes <- data.frame(name = unique(c(sankey_data$LABEL_EN, sankey_data$next_specialty)))
links <- sankey_data %>%
    mutate(source = match(LABEL_EN, nodes$name) - 1,
           target = match(next_specialty, nodes$name) - 1,
           value = count)

sankey <- sankeyNetwork(
    Links = links, Nodes = nodes,
    Source = "source", Target = "target",
    Value = "value", NodeID = "name",
    units = "Transitions", fontSize = 12, nodeWidth = 30,
    sinksRight = FALSE
)

saveNetwork(sankey, file = "sankey.html")


# Remove the specialty codes that are not relevant
valvira_only_special <- valvira %>%
    mutate(LABEL_EN = ifelse(LABEL_EN %in%
        c("LICENSED_MED", "LICENSED_DENT", "STUDENT_MED", "STUDENT_DENT"), NA, LABEL_EN)) %>% 
    filter(!is.na(LABEL_EN))

# longest_specialty <- valvira_only_special %>%
#     mutate(practicing_days = as.numeric(difftime(pmin(END_DATE, as.Date("2023-01-01")),
#         START_DATE,
#         units = "days"
#     ))) %>%
#     # get the practicing days for each doctor for each specialty
#     group_by(FID, LABEL_EN) %>%
#     summarise(
#         practicing_days = sum(practicing_days)
#     ) %>%
#     # pick the longest practicing specialty for each doctor
#     arrange(FID, desc(practicing_days)) %>%
#     slice(1) %>%
#     select(FID, SPECIALTY = LABEL_EN)

last_specialty <- valvira_only_special %>%
    arrange(FID, desc(START_DATE)) %>%
    group_by(FID) %>%
    slice(1) %>%
    select(FID, LAST_SPECIALTY = LABEL_EN)

# merge longest speciality with the doctors dataset
valvira_primus <- fread("/media/volume/Projects/DSGELabProject1/doctor_characteristics_wlongest_Specialty_20250220.csv")
valvira_primus <- valvira_primus %>%
    left_join(spec_dict %>% select(-COMPRISED), by = c("SPECIALTY" = "CODEVALUE")) %>%
    select(-SPECIALTY) %>% 
    dplyr::rename(LONGEST_SPECIALTY = LABEL_EN) %>% 
    mutate(LABEL_EN = ifelse(LONGEST_SPECIALTY %in%
        c("LICENSED_MED", "LICENSED_DENT", "STUDENT_MED", "STUDENT_DENT"), NA, LONGEST_SPECIALTY)) %>%
    left_join(last_specialty, by = "FID")