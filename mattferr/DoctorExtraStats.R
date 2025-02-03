# LAST UPDATED: 2025-02-03
library(data.table)
library(ggplot2)

# SET PATHS HERE:
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
Valvira_header_path = "~/Desktop/shared-directory/data_dictionaries/Valvira/FD_2698_Liite1"
outpath_df = "/media/volume/Projects/mattferr/extra_doctor_info_20250203.csv"

# fetch data
df = fread(Valvira_path, encoding = "Latin-1")
header = fread(Valvira_header_path)
colnames(df) = header$new_name

# extract the following information:
# - profession duration
# - latest degree
# - latest profession

df[, PROFESSION_START_DATE := as.Date(PROFESSION_START_DATE, format="%d.%m.%Y")]
df[, PROFESSION_END_DATE := as.Date(PROFESSION_END_DATE, format="%d.%m.%Y")]
df[, PROFESSION_END_DATE := pmin(PROFESSION_END_DATE, as.Date("2023-01-01"))]
df[, DEGREE_DATE := as.Date(DEGREE_DATE, format="%d.%m.%Y")]

DAYS_IN_YEAR = 365.25
df[, profession_duration := as.numeric(difftime(max(PROFESSION_END_DATE, na.rm = TRUE), min(PROFESSION_START_DATE, na.rm = TRUE), units="days")) / DAYS_IN_YEAR, by = ID]
df[, latest_degree := DEGREE[which.max(DEGREE_DATE)], by = ID]
df[, latest_profession := PROFESSION[which.max(PROFESSION_END_DATE)], by = ID]

# export dataset
df = df[, .(ID, profession_duration, latest_degree, latest_profession)]
df = unique(df)
write.csv(df, outpath_df, row.names = FALSE)
