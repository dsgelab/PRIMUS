library(data.table)
library(dplyr)

# SET PATHS HERE:
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
Valvira_header_path = "~/Desktop/shared-directory/data_dictionaries/Valvira/FD_2698_Liite1"

# select hilmo or avohilmo
#THL_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/FD_2698_THL2023_2698_AH_11.csv"
#THL_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/FD_2698_THL2023_2698_HILMO0712.csv"

# fetch data (Valvira + THL)
#df1 = fread(Valvira_path, nrows=1000, encoding = "Latin-1") # TESTING
#df2 = fread(THL_path, nrows=1000, encoding = "Latin-1") # TESTING
#df1 = fread(Valvira_path, encoding = "Latin-1")
#df2 = fread(THL_path, encoding = "Latin-1")
header1 = fread(Valvira_header_path)$new_name
colnames(df1) = header1
df2 = df2 %>% rename(FD_HASH_CODE = FD_HASH_Rekisteröinti..numero)

# filter data
# 1. keep only latest profession (based on start date) for each Valvira doctor
# 2. remove missing hash keys from THL visits 
nrow(df1)
df1_filtered = df1[order(-PROFESSION_START_DATE), .SD[1], by=ID]
nrow(df1_filtered)/nrow(df1)*100

nrow(df2)
df2_filtered = df2[(!is.na(FD_HASH_CODE)) & (FD_HASH_CODE!="PUUTTUVA"),]
nrow(df2_filtered)/nrow(df2)*100

# inner join the two datasets using data.table for faster performance
setkey(df1_filtered, FD_HASH_CODE)
setkey(df2_filtered, FD_HASH_CODE)
df = df1_filtered[df2_filtered, nomatch=0]

# extract summary stastistics
length(unique(df$ID))
length(unique(df$ID))/length(unique(df1$ID))*100
length(unique(df$FID))
length(unique(df$FID))/length(unique(df2$FID))*100
