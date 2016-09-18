##############################################################################################################
####################                Difference in Ox - General vs Roadside                ####################
##############################################################################################################

source('!!_Setup.R')
source('!!_Functions.R')
source('!!_parameters.R')



pollutant.pick = c("o3_ug", "no2_ug", "nox_ug") # pollutants to pick
airYear = 2010:2016 # year to be looked at
air.hr = LoadAirHour(yearVec = airYear, pollutant.pick = pollutant.pick)
rollHours = 8760 #hours of rolling

air.hr$site = substr(air.hr$site, 1, 2)
air.hr = air.hr %>% mutate(Ox = o3_ug/p_O3 + no2_ug/p_NO2)

air.hr$siteType = "General"
air.hr$siteType[which(air.hr$site == 'MB')] = NA
air.hr$siteType[which(air.hr$site == 'CB')] = 'Roadside'
air.hr$siteType[which(air.hr$site == 'CL')] = 'Roadside'
air.hr$siteType[which(air.hr$site == 'MK')] = 'Roadside'

air.hr.Cast = cast(air.hr, formula = date~siteType, fun.aggregate = function(x) mean(x, na.rm = T), value = 'Ox')

a1 = air.hr.Cast %>% mutate(diff.Ox = Roadside - General) %>% select(date, diff.Ox)

airhr.Group.Rolling = rollingMean(a1, pollutant = "diff.Ox", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'siteType')
airhr.Group.Rolling = selectByDate(airhr.Group.Rolling, year = 2012:2019)

library(DBI)
library(RMySQL)





## Setup DB connection
con <- dbConnect(MySQL(), user="root", password="ckk%i9==", dbname="airdata", host="localhost")
dbListTables(con)
names(airhr.Group.Rolling) = c('date', 'temp', 'OxRun')
dbWriteTable(con, name = 'ox8760', airhr.Group.Rolling[c(1,3)], row.names = F, overwrite = T,
             field.types=list(date="datetime", OxRun="float"))





###### ---- Rolling NOx for roadside ---- ######

source('setup.R')
source('Utilities.R')
source('functions.R')
source('parameters.R')
logEnable = T

### -- Need to FIll inside #############################################
rollHours = 8760 #hours of rolling
pollutant.pick = c("date", "site","o3_ug", "no2_ug", "nox_ug", 'co_ug') # pollutants to pick
airYear = 2010:2016 # year to be looked at
### -- Need to FIll inside #############################################


air.hr = LoadAirHour(dir_AirHour, dir_Master, airYear, pollutant.pick)

m2 = air.hr %>% filter(site == 'MK_HK' | site == 'CB_HK' | site == 'CL_HK' |
  site == 'MB_HK' | site == 'TC_HK') %>% mutate(no_ug = (nox_ug - no2_ug)/p_NO2*p_NO)

m3 = rollingMean(m2, pollutant = "o3_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site')
m3 = rollingMean(m3, pollutant = "nox_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site')
m3 = rollingMean(m3, pollutant = "no2_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site')
m3 = rollingMean(m3, pollutant = "no_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site')
m3 = rollingMean(m3, pollutant = "co_ug", 
  align = "right", width = rollHours, data.thresh = 66.66, type = 'site')

m3$site = substr(m3$site, 1, 2)
airYear2 = airYear
airYear2[1] = airYear2[1]+1
m3 = selectByDate(m3, year = airYear2)
rm(airYear2)

m4a = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'o3_ug', sep = ''))
names(m4a) = c('date', 'CB_O3', "CL_O3", "MB_O3", "MK_O3", "TC_O3")

m4b = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'nox_ug', sep = ''))
names(m4b) = c('date', 'CB_NOX', "CL_NOX", "MB_NOX", "MK_NOX", "TC_NOX")
m4b = m4b %>% select(-date)

m4c = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'no2_ug', sep = ''))
names(m4c) = c('date', 'CB_NO2', "CL_NO2", "MB_NO2", "MK_NO2", "TC_NO2")
m4c = m4c %>% select(-date)

m4d = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'no_ug', sep = ''))
names(m4d) = c('date', 'CB_NO', "CL_NO", "MB_NO", "MK_NO", "TC_NO")
m4d = m4d %>% select(-date)

m4e = cast(m3, formula = date~site, value = paste('rolling', rollHours, 'co_ug', sep = ''))
names(m4e) = c('date', 'CB_CO', "CL_CO", "MB_CO", "MK_CO", "TC_CO")
m4e = m4e %>% select(-date)

m4 = cbind.data.frame(m4a, m4b, m4c, m4d, m4e)

m5 = m4[c('date',	'MB_O3',	'TC_O3',	'CB_NOX',	'CL_NOX',	'MK_NOX',	'CB_NO2',	'CL_NO2',
  'MK_NO2',	'CB_NO',	'CL_NO',	'MK_NO', 'CB_CO',	'CL_CO',	'MK_CO')]


WriteCSV_ToFolder(dir_RollingNO2, dir_Master, m5, "rolling.csv")


