
if (Sys.info()["nodename"] == "PONG-PC"){
  dir_Prefix = "C:/Users/Frankie/Dropbox/"
} else { 
  dir_Prefix = "C:/Users/poas31/Dropbox/"}

library("openair")
library("reshape")
library("reshape2")
library("stringr")
library("ggplot2")
library("grid")
library("gridExtra")
library("latticeExtra")
library("dplyr")

dir_Master = getwd()

dir_AirHour = paste(dir_Prefix, "01_Documents/05_EPD/00_rawData/01_air_Data/01_airHour", sep = "")
dir_AQHI = paste(dir_Prefix, "01_Documents/05_EPD/00_rawData/03_AQHI_data", sep = "")
dir_Wind = paste(dir_Prefix, "01_Documents/05_EPD/00_rawData/02_wind_Data", sep = "")

dir_West_v_East = paste(dir_Prefix, 
  "01_Documents/05_EPD/21_Project Studies/!_Analysis/West_V_East", sep = "")
dir_RoadOx = paste(dir_Prefix, 
  "01_Documents/05_EPD/21_Project Studies/!_Analysis/RoadvsGeneral_Ox", sep = "")