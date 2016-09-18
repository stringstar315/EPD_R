source('!!_Setup.R')
source('!!_fn-csvLoad.R')
source('!!_fn-routines.R')
source('!!_fn-analysis.R')

## Rountine Updates 
Format_EPIC_ENVISTA(fmt_EPIC = T, fmt_EVIS = T, bindFiles = T, bindAQHI = T) ## Gen combined CSV
UpdateDB_AQHI_FULL(GenSQL = T, GenCSV = T) ## update AQHI data base and CSV

UpdateDB_Met_FULL(GenCIS = T, GenHour = T)
UpdateDB_airPRD() 
UpdateDB_airHK.FULL(yrTbl.NewYr = T, newYear = 2016, yearVec = 2010:2016)   ##hourly from 2010 to 2016
UpdateDB_HealthRisk(yearVec = 2014:2016)

#######
GenAAQI(date.start = '1/6/2015', date.end = '31/5/2016')
Format_HKO_hour() #update from requested HKO hr data (may need adjustment)

## Analysis
episode.DayMax(yearVec = 2014:2016)



  
  
setwd(dir_Master)  
df = as.data.frame(read.csv('cis.csv', na.strings = '\\N'))
df.melt = melt(df, id = c('year', 'day', 'type', 'site'))
write.csv(df.melt, 'cis_melt.csv', na = '\\N', row.names = F)
