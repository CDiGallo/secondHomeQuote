# library(tidyverse)
# 
# 
# 
# zweitwohnungen <- read.csv("Zweitwohnungen_Gemeinden.csv",sep=";", encoding = "utf-8")
# 
# zweitwohnungen <- read_csv2("LINDAS wichtige Dokumente/are/zweitwohnungen_Gemeinden.csv")
# 
# 
# zweitwohnungen
# to_join <- read.csv("tojoin.csv",sep=";")
# 
# to_join$id <- to_join$ï..id
# 
# 
# zweit_wohnungen_fixed <- left_join(zweitwohnungen,to_join, by=c("Verfahren"="id"))
# 
# 
# 
# zweit_wohnungen_fixed$Erstwohnungsanteil <- zweit_wohnungen_fixed$Erstwohnungsanteil/100
# 
# 
# write.csv2(zweit_wohnungen_fixed, "zweitwohnungen_gemeinden_fixed.csv")
# 
# 
# ?left_join
# 
# 
# 
# 
# 
# 
# # generalizing ------------------------------------------------------------

install.packages("ggplot2")
library(readxl)
library(tidyverse)
library(readr)
library(dplyr)
setwd("C:/Users/U80867579/Documents/LINDAS")

setwd("\\\\adb.intra.admin.ch/Userhome$/BAR-01/U80867579/data/Documents/LINDAS wichtige Dokumente/are/raw_data")


# list_file <- list.files(pattern = "*.xlsx") %>% 
#   lapply(read_excel, sheet=2) %>% 
#   bind_rows 
#list_file





Data1 <- read_excel("ZWG_2019_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2019-03-01")

Data2 <- read_excel("ZWG_2019_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2019-10-01")

df <- add_row(
  Data1,
  Data2
)
df

Data1 <- read_excel("ZWG_2020_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2020-03-01")

Data2 <- read_excel("ZWG_2020_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2020-10-01")

df2 <- add_row(
  Data1,
  Data2
)


Data1 <- read_excel("ZWG_2021_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2021-03-01")

Data2 <- read_excel("ZWG_2021_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2021-10-01")

df3 <- add_row(
  Data1,
  Data2
)

Data1 <- read_excel("ZWG_2022_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2022-03-01")

Data2 <- read_excel("ZWG_2022_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2022-10-01")

df4 <- add_row(
  Data1,
  Data2
)

Data1 <- read_excel("ZWG_2023_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2023-03-01")


df <- add_row(
  df,
  Data1
)

df <- add_row(
  df,
  df2
)


df <- add_row(
  df,
  df3
)


df <- add_row(
  df,
  df4
)

#adding the first one which is irregular:

# Data2 <- read_excel("ZWG_2018.xlsx", sheet = 2) %>% add_column("Datum"="2018-12-31", "Status"= NA) %>% rename(Kt_Kz=KT, Verfahren=ZWG_3200)
# 
# df <- add_row(
# df,
# Data2)
# 
# 
# #for the time being Verfahren will be excluded, as it is not campatible with 2018
df <- df %>% select(-"Status")



write_csv("Zweitwohnungen_file_gemeinden.csv",x = df)




# Aggregation -------------------------------------------------------------


verfahren <- df %>% group_by(Kt_Kz,Datum) %>%  count(Verfahren)

anz_wohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(Anzahlwohnungen= sum(ZWG_3150))


anz_erstwohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(Anzahl_Erstwohnungen= sum(ZWG_3010))


erstwohnung_like <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(Anzahl_Erstwohnungsgleiche= sum(ZWG_3100))

erstwohnung_anteil <- anz_wohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(anteil_Erstwohnungen= mean(ZWG_3110))
zweitwohnung_anteil <- anz_wohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(anteil_zweitwohnungen= mean(ZWG_3120))


anz_wohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(Anzahlwohnungen= sum(ZWG_3150))
anz_wohnungen
df$ZWG_3150

join <- left_join(verfahren,anz_wohnungen, by=c("Kt_Kz","Datum"))
join

join <- left_join(join,anz_erstwohnungen, by=c("Kt_Kz","Datum"))
join <- left_join(join,erstwohnung_anteil, by=c("Kt_Kz","Datum"))
join <- left_join(join,zweitwohnung_anteil, by=c("Kt_Kz","Datum"))

join
kt <- df %>% select(Kt_No,Kt_Kz) %>% unique()

join <- left_join(join,kt, by=c("Kt_Kz"))


write_csv("Zweitwohnungen_file_kantone.csv",x = join)



# 
# library(readxl)
# library(tidyverse)
# library(readr)
# 
# 
# 
# setwd("\\\\adb.intra.admin.ch/Userhome$/BAR-01/U80867579/data/Documents/LINDAS wichtige Dokumente/are/raw_data")
# 
# 
# # list_file <- list.files(pattern = "*.xlsx") %>% 
# #   lapply(read_excel, sheet=2) %>% 
# #   bind_rows 
# #list_file
# 
# 
# 
# Data1 <- read_excel("ZWG_2019_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2019-03-01")
# 
# Data2 <- read_excel("ZWG_2019_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2019-10-01")
# 
# df <- add_row(
#   Data1,
#   Data2
# )
# df
# 
# Data1 <- read_excel("ZWG_2020_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2020-03-01")
# 
# Data2 <- read_excel("ZWG_2020_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2020-10-01")
# 
# df2 <- add_row(
# Data1,
#   Data2
# )
# 
# 
# Data1 <- read_excel("ZWG_2021_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2021-03-01")
# 
# Data2 <- read_excel("ZWG_2021_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2021-10-01")
# 
# df3 <- add_row(
#   Data1,
#   Data2
# )
# 
# Data1 <- read_excel("ZWG_2022_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2022-03-01")
# 
# Data2 <- read_excel("ZWG_2022_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2022-10-01")
# 
# df4 <- add_row(
#   Data1,
#   Data2
# )
# 
# Data1 <- read_excel("ZWG_2023_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2023-03-01")
# 
# 
# df <- add_row(
#   df,
#   Data1
# )
# 
# df <- add_row(
#   df,
#   df2
# )
# 
# 
# df <- add_row(
#   df,
#   df3
# )
# 
# 
# df <- add_row(
#   df,
#   df4
# )
# 
# #adding the first one which is irregular:
# 
# # Data2 <- read_excel("ZWG_2018.xlsx", sheet = 2) %>% add_column("Datum"="2018-12-31", "Status"= NA) %>% rename(Kt_Kz=KT, Verfahren=ZWG_3200)
# # 
# # df <- add_row(
# # df,
# # Data2)
# # 
# # 
# # #for the time being Verfahren will be excluded, as it is not campatible with 2018
#  df <- df %>% select(-"Status")
# 
# 
# 
# write_csv("Zweitwohnungen_file_gemeinden.csv",x = df)
# 
# 
# 
# 
# # Aggregation -------------------------------------------------------------
# 
# 
# verfahren <- df %>% group_by(Kt_Kz,Datum) %>%  count(Verfahren)
# 
# anz_wohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(Anzahlwohnungen= sum(ZWG_3150))
# 
# 
# anz_erstwohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(Anzahl_Erstwohnungen= sum(ZWG_3010))
# 
# 
# erstwohnung_like <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(Anzahl_Erstwohnungsgleiche= sum(ZWG_3100))
# 
# erstwohnung_anteil <- anz_wohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(anteil_Erstwohnungen= mean(ZWG_3110))
# zweitwohnung_anteil <- anz_wohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(anteil_zweitwohnungen= mean(ZWG_3120))
# 
# 
# anz_wohnungen <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(Anzahlwohnungen= sum(ZWG_3150))
# anz_wohnungen
# df$ZWG_3150
# 
# join <- left_join(verfahren,anz_wohnungen, by=c("Kt_Kz","Datum"))
# join
# 
# join <- left_join(join,anz_erstwohnungen, by=c("Kt_Kz","Datum"))
# join <- left_join(join,erstwohnung_anteil, by=c("Kt_Kz","Datum"))
# join <- left_join(join,zweitwohnung_anteil, by=c("Kt_Kz","Datum"))
# 
# join
# kt <- df %>% select(Kt_No,Kt_Kz) %>% unique()
# 
# join <- left_join(join,kt, by=c("Kt_Kz"))
# 
# 
# write_csv("Zweitwohnungen_file_kantone.csv",x = join)

# firstData <- firstData %>% add_column("Jahr"=2019)
# 
# 
# year <- c(2019,2020,2021,2022,2023)
# quarter <- c(1,3)
# 
# 
# file <- paste0("LINDAS wichtige Dokumente/are/raw_data/ZWG_",2019,"_Q",j,".xlsx")
# file <- file %>% add_column("Jahr"=f)
# 
# firstData <- add_row(firstData,file)
# 
# 
# for( f in  year ){
#     for( j in quarter){
#       
#       file <- paste0("LINDAS wichtige Dokumente/are/raw_data/ZWG_",f,"_Q",j,".xlsx")
#       file <- file %>% add_column("Jahr"=f)
#       
#       firstData <- add_row(firstData,file)
#     
#     }
# }
# file <- paste0("LINDAS wichtige Dokumente/are/raw_data/ZWG_",f,"_Q",j,".xlsx")
# 
# my_data <- read_excel(file, sheet = 2)
# 
# 
# add_row(firstData,my_data)
# my_data
# read_csv2("LINDAS wichtige Dokumente/are/raw_data/ZWG_2018.xlsx")
