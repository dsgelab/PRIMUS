library(data.table)

# SET PATHS HERE:
file_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
header_path = "~/Desktop/shared-directory/data_dictionaries/Valvira/FD_2698_Liite1"
outpath = "/media/volume/Projects/mattferr/FilteredDoctorList_20250120.txt"

# fetch data
df = fread(file_path, encoding = "Latin-1")
header = fread(header_path)
colnames(df) = header$new_name

# total number of doctors
total_doctors = unique(df$ID)
length(total_doctors)

# select profession, considering only the following:
# laillistettu erikoislääkäri – licensed specialist doctor
# laillistettu lääkäri – licensed doctor

licensed_doctor =  unique(
    df[df$PROFESSION %in% c("laillistettu erikoislääkäri", "laillistettu lääkäri"),]$ID
)
length(licensed_doctor)
length(licensed_doctor)*100/length(total_doctors)

# select doctors actively working
DATE_THRESHOLD = as.Date("01.01.2023", format="%d.%m.%Y")
df$PROFESSION_END_DATE = as.Date(df$PROFESSION_END_DATE, format="%d.%m.%Y")
active_doctors = unique(
    df[df$PROFESSION_END_DATE > DATE_THRESHOLD,]$ID
)
length(active_doctors)
length(active_doctors)*100/length(total_doctors)

# generate final list of doctors:
# doctors who are licensed and actively working
DoctorList = intersect(licensed_doctor, active_doctors)
length(DoctorList)
length(DoctorList)*100/length(total_doctors)

# export list
write.table(data.frame(DoctorList), outpath, row.names = FALSE, col.names = FALSE)
