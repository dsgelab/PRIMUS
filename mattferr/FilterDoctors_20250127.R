library(data.table)
library(ggplot2)

# SET PATHS HERE:
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
Valvira_header_path = "~/Desktop/shared-directory/data_dictionaries/Valvira/FD_2698_Liite1"
SF_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Tilastokeskus/FD_2698_ksyyt_findata_2698.csv"
outpath = "/media/volume/Projects/mattferr/FilteredDoctorList_20250127.txt"

# fetch data
df = fread(Valvira_path, encoding = "Latin-1")
header = fread(Valvira_header_path)
colnames(df) = header$new_name

death = fread(SF_path, encoding = "Latin-1")
death = death[,c('FID','kuolpv')]

# total number of doctors
total_doctors = unique(df$ID)
length(total_doctors)

# 1. select only last license for each doctor
df_last = df[order(-PROFESSION_START_DATE), .SD[1], by=ID]
length(unique(df_last$ID)) # check: equal to total_doctors

# # plot license date (start+end) distributions
# df_plot = rbind(data.frame(Date = df_last$PROFESSION_START_DATE, Type = "License Start Date"),data.frame(Date = df_last$PROFESSION_END_DATE, Type = "License End Date")
# ggplot(df_plot, aes(x = Date, fill = Type)) +
#     geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
#     labs(x = "Date", y = "Count") +
#     theme_minimal()
# # plot license duration distribution
# df_last$duration = as.numeric(difftime(df_last$PROFESSION_END_DATE, df_last$PROFESSION_START_DATE, units = "days"))
# ggplot(df_last[df_last$duration>0], aes(x = duration, fill = PROFESSION)) +
#     geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
#     labs(x = "License duration (days)", y = "Count") +
#     theme_minimal()

# then select only those that are :
# 1. licensed = "laillistettu"
# 2. still active (PROFESSION_END_DATE > "01.01.2023")
# 3. obtained more than 5 years ago (PROFESSION_START_DATE < "01.01.2018")
df_last$PROFESSION_START_DATE = as.Date(df_last$PROFESSION_START_DATE, format = "%d.%m.%Y")
df_last$PROFESSION_END_DATE = as.Date(df_last$PROFESSION_END_DATE, format = "%d.%m.%Y")

# Filter rows whose 'profession' starts with "laillistettu"
LICENSE_REGEX = grep("^laillistettu", df_last$PROFESSION)
df_licensed = df_last[LICENSE_REGEX,]
length(unique(df_licensed$ID))
length(unique(df_licensed$ID))*100/length(total_doctors)

df_active = df_licensed[PROFESSION_END_DATE > as.Date("01.01.2023", format = "%d.%m.%Y"),]
length(unique(df_active$ID))
length(unique(df_active$ID))*100/length(total_doctors)

df_active2 = df_active[PROFESSION_START_DATE < as.Date("01.01.2018", format = "%d.%m.%Y"),]
length(unique(df_active2$ID))
length(unique(df_active2$ID))*100/length(total_doctors)

# filter out doctors that are dead
death$DEATH_DATE = as.Date(death$kuolpv, format = "%Y-%m-%d")
death = death[death$DEATH_DATE < as.Date("01.01.2023", format = "%d.%m.%Y"),]
df_active_alive = df_active2[!(ID %in% death$FID)]
length(unique(df_active_alive$ID))
length(unique(df_active_alive$ID))*100/length(total_doctors)

# # plot license duration distribution
# df_active_alive$duration = as.numeric(difftime(df_active_alive$PROFESSION_END_DATE, df_active_alive$PROFESSION_START_DATE, units = "days"))
# ggplot(df_active_alive[df_active_alive$duration>0], aes(x = duration, fill = PROFESSION)) +
#     geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
#     labs(x = "License duration (days)", y = "Count") +
#     theme_minimal()

# export list
DoctorList = unique(df_active_alive$ID)
write.table(data.frame(DoctorList), outpath, row.names = FALSE, col.names = FALSE)
