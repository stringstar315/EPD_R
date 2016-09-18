##############################################################################################################
####################                              Sys.info()                              ####################
##############################################################################################################
##############################################################################################################

if (Sys.info()["nodename"] == "PONG-PC"){
  dir_Prefix = "C:/Users/Frankie/Dropbox/"
} else if (Sys.info()["nodename"] == "POAS31-RT33"){ 
  dir_Prefix = "C:/Users/poas31/Dropbox/"}

cur_Year = as.numeric(format.Date(Sys.time(), "%Y"))
cur_Month = as.numeric(format.Date(Sys.time(), "%m"))
cur_Date = as.character(format.Date(Sys.time(), "%Y-%m-%d"))
cur_Time = as.character(format.Date(Sys.time(), "%H:%M"))



##############################################################################################################
####################                              AQMS Order                              ####################
##############################################################################################################

RoadsideCluster = c('CB', 'CL', 'MK')
UrbanCluster = c('CW', 'EN', 'KC', 'KT', 'SP', 'TK', 'TW')
EasternCluster = c('TP', 'ST', 'MB')
WesternCluster = c('TC', 'TM', 'YL')
GeneralCluster = c(UrbanCluster, WesternCluster, EasternCluster)
ALLCluster = c(RoadsideCluster, UrbanCluster, WesternCluster, EasternCluster)




##############################################################################################################
####################                     List of Library to be Loaded                     ####################
##############################################################################################################

library("dplyr")
library("openair")
library("reshape")
library("stringr")
library("ggplot2")
#install.packages('lubridate')
library('lubridate')


library("zoo")
library('XML')


library("grid")
library("gridExtra")
library("latticeExtra")

library("ggvis")
library("class")
library('gmodels')
library('MESS') ##using auc() i.e. area under curve
#source('!!_Install_Packages.R')


##############################################################################################################
####################                         List of Directories                          ####################
##############################################################################################################
##############################################################################################################

dir_Master = getwd()

dir_airHK = paste(dir_Prefix, "02_DataArchive/01airHK_csv", sep = "")
dir_windHK = paste(dir_Prefix, "02_DataArchive/02windHK_csv", sep = "")
dir_airPRD = paste(dir_Prefix, "02_DataArchive/03airPRD_csv", sep = "")

dir_AAQI = paste(dir_Prefix, "02_DataArchive/AAQI", sep = "")




dir_AirHour = paste(dir_Prefix, "01_Documents/05_EPD/00_rawData/01_air_Data/01_airHour", sep = "")
dir_AQHI = paste(dir_Prefix, "01_Documents/05_EPD/00_rawData/03_AQHI_data", sep = "")
dir_Wind = paste(dir_Prefix, "01_Documents/05_EPD/00_rawData/02_wind_Data", sep = "")
dir_PRD = paste(dir_Prefix, "01_Documents/05_EPD/00_rawData/04_PRD_Data", sep = "")


dir_EpisodeDays = paste(dir_Prefix, 
  "01_Documents/05_EPD/21_Project Studies/!_Analysis/EpisodeDays", sep = "")



dir_Wind.cis = paste(dir_Prefix, "01_Documents/05_EPD/00_rawData/02_wind_Data/HKO_CIS", sep = "")


dir_West_v_East = paste(dir_Prefix, 
  "01_Documents/05_EPD/21_Project Studies/!_Analysis/West_V_East", sep = "")
dir_RoadOx = paste(dir_Prefix, 
  "01_Documents/05_EPD/21_Project Studies/!_Analysis/RoadvsGeneral_Ox", sep = "")
dir_RoadNO2 = paste(dir_Prefix, 
  "01_Documents/05_EPD/21_Project Studies/!_Analysis/2016_Compare Roadside NO2", sep = "")
dir_RollingNO2 = paste(dir_Prefix, 
  "01_Documents/05_EPD/21_Project Studies/!_Analysis/2016_07_RollingNO2", sep = "")
dir_RollingCO = paste(dir_Prefix, 
  "01_Documents/05_EPD/21_Project Studies/!_Analysis/2016_07_RollingCO", sep = "")
dir_StatSummary = paste(dir_Prefix, 
  "01_Documents/05_EPD/21_Project Studies/!_Analysis/Statistical_Summary", sep = "")


dir_SQL.csv = paste(dir_Prefix, "02_RawData/csv", sep = "")


dir_Options = paste(dir_Prefix, 
  "01_Documents/06_Stocks", sep = "")



















