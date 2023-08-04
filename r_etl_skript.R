 
# # generalizing ------------------------------------------------------------


library(readxl)
library(tidyverse)
library(readr)
library(dplyr)

setwd("C:/Users/U80867579/Documents/LINDAS")

setwd("\\\\adb.intra.admin.ch/Userhome$/BAR-01/U80867579/data/Documents/LINDAS wichtige Dokumente/are/raw_data")
setwd("C:/Users/claudio/Documents/Projekte/Zweitwohnungen/raw_data")

# list_file <- list.files(pattern = "*.xlsx") %>% 
#   lapply(read_excel, sheet=2) %>% 
#   bind_rows 
#list_file


Data <- read_excel("ZWG_2018.xlsx", sheet = 2 ) # Here because the "ifelse" needs it to be loaded
Data <- read_excel("ZWG_2018.xlsx", sheet = 2 ) %>% add_column("Status" = ifelse(Data$ZWG_3200 %in% c(1,3), 0,1))%>% add_column("Datum"="2018-01-01") 

Data

Data1 <- read_excel("ZWG_2019_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2019-03-31")

# explains, that their excel file explains it wrong. staus 1 means over 20% status 0 means less than 20% second homes
#f <- Data1  %>% add_column("over_limit" = ifelse(Data1$Verfahren %in% c(1,3,5,7), 0,1))
#sum(f$Verfahren==f$Status)


Data2 <- read_excel("ZWG_2019_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2019-10-31")
Data2

df <- add_row(
  Data1,
  Data2
)
df

Data1 <- read_excel("ZWG_2020_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2020-03-31")

Data2 <- read_excel("ZWG_2020_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2020-10-31")

df2 <- add_row(
  Data1,
  Data2
)


Data1 <- read_excel("ZWG_2021_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2021-03-31")

Data2 <- read_excel("ZWG_2021_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2021-10-31")

df3 <- add_row(
  Data1,
  Data2
)

Data1 <- read_excel("ZWG_2022_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2022-03-31")

Data2 <- read_excel("ZWG_2022_Q3.xlsx", sheet = 2) %>% add_column("Datum"="2022-10-31")

df4 <- add_row(
  Data1,
  Data2
)

Data1 <- read_excel("ZWG_2023_Q1.xlsx", sheet = 2) %>% add_column("Datum"="2023-03-31")


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

Canton_list <- df %>% select(Kt_No,Kt_Kz) %>% unique() #this is the referencelist of cantons which each identifier
Canton_list
Data <-  left_join(Data,Canton_list, by= c("KT"="Kt_Kz")) %>% rename(Kt_Kz=KT) %>% rename(Verfahren = ZWG_3200) #we join the  Canton_identifier to the first excel.



df <- add_row(
  df,
  Data
)

df <- df %>% mutate(shared_muni=Gem_No) %>% select(-Name) #name comes from shared dimension. and numbers are better to join.





write_csv("Zweitwohnungen_file_gemeinden_every_year.csv", x=df)




# Aggregation -------------------------------------------------------------
#Average Status in Canton and average zweitwohnungsanteil.

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
zweitwohnung_anteil <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(kantonaler_zweitwohnungsanteil= mean(ZWG_3120))
zweitquote <- df %>% group_by(Kt_Kz,Datum) %>%  summarise(kantonaler_statusprozent= mean(Status))

aggregation <- left_join(zweitwohnung_anteil,zweitquote, by=c("Kt_Kz","Datum"))
zweitwohnung_anteil

aggregation <- left_join(df,aggregation, by=c("Kt_Kz","Datum"))
aggregation
aggregation <- aggregation %>% rename(Gesamtzahl_aller_wohnungen=ZWG_3150, Zweitwohnungsanteil=ZWG_3120, Erstwohnungsanteil= ZWG_3110, Anzahlerstwohnungen_gleichgestellter_wohnungen=ZWG_3100, Anzahl_Erstwohnungen=ZWG_3010)


write_csv("Zweitwohnungen_file_gemeinden_every_year_aggregation.csv", x=aggregation)

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
# 
# 

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
