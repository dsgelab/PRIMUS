library(data.table)
library(ggplot2)

# SET PATHS HERE:
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
Valvira_header_path = "~/Desktop/shared-directory/data_dictionaries/Valvira/FD_2698_Liite1"
outpath_ids = "/media/volume/Projects/mattferr/FilteredDoctorList_20250128.txt"
outpath_df = "/media/volume/Projects/mattferr/FilteredDoctorDataset_20250128.csv"

# fetch data
df = fread(Valvira_path, encoding = "Latin-1")
header = fread(Valvira_header_path)
colnames(df) = header$new_name

# total number of doctors
total_doctors = unique(df$ID)
length(total_doctors)

# filter doctors with medical license
# medical license = PROFESSION starts with “laillistettu”
LICENSE_REGEX = grep("^laillistettu", df$PROFESSION)
df_licensed = df[LICENSE_REGEX,]
length(unique(df_licensed$ID))
length(unique(df_licensed$ID))*100/length(total_doctors)

# evaluate the total license time
# [ NB 1 ] a doctor can have multiple licenses in their professional career (eg. both general practitioner and specialist), will need to sum different profession durations
# [ NB 2 ] licenses can be active at the same time, will need to adjust the profession start and end date for a precise calculation of the total license duration
# [ NB 3 ] last license can last until the year 2173, we will cut the profession end date at 01.01.2023 
# [ NB 4 ] some doctors have multiple degrees referencing the same profession (specialty + sub-specialty degrees), for each license we will then only take the first profession available (and its start date)
df_licensed$PROFESSION_START_DATE = as.Date(df_licensed$PROFESSION_START_DATE, format = "%d.%m.%Y")
df_licensed$PROFESSION_END_DATE = as.Date(df_licensed$PROFESSION_END_DATE, format = "%d.%m.%Y")
CUTOFF_DATE = as.Date("01.01.2023", format = "%d.%m.%Y")

#adjust analysis (see NB comments)
df_licensed = df_licensed[order(ID, PROFESSION_START_DATE)]
df_licensed = df_licensed[, .(
    PROFESSION_START_DATE = min(PROFESSION_START_DATE), 
    PROFESSION_END_DATE = max(PROFESSION_END_DATE)
    ), by = .(ID, PROFESSION)]
df_licensed[, PROFESSION_END_DATE := pmin(PROFESSION_END_DATE, shift(PROFESSION_START_DATE, type = "lead", fill = CUTOFF_DATE) - 1), by = ID]
df_licensed[, PROFESSION_START_DATE := shift(PROFESSION_END_DATE, type = "lag", fill = min(PROFESSION_START_DATE)), by = ID]
df_licensed$PROFESSION_END_DATE[df_licensed$PROFESSION_END_DATE > CUTOFF_DATE] = CUTOFF_DATE

df_licensed$duration = as.numeric(difftime(df_licensed$PROFESSION_END_DATE, df_licensed$PROFESSION_START_DATE, units = "days"))
df_licensed_sum = df_licensed[, .(total_duration = sum(duration)), by = ID]

# export dataset
write.csv(df_licensed, outpath_df, row.names = FALSE)

#plot license duration distribution
DAYS_IN_YEAR = 365.25
ggplot(df_licensed_sum, aes(x = total_duration/DAYS_IN_YEAR)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
    geom_vline(xintercept = 5, color = "red", linetype = "dashed", size = 1) +
    labs(x = "License duration (Years)", y = "Count") +
    theme_minimal()

# extract list of doctors with a total license duration of at least 5 years
df_final = df_licensed_sum[total_duration > 5*DAYS_IN_YEAR,]
length(unique(df_final$ID))
length(unique(df_final$ID))*100/length(total_doctors)

# export list
DoctorList = unique(df_final$ID)
write.table(data.frame(DoctorList), outpath_ids, row.names = FALSE, col.names = FALSE)